{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- Needed for `SimEvent` type.
{-# OPTIONS_GHC -Wno-partial-fields    #-}
-- `MonadMaskingState` is deprecated in `io-classes`, but we provide an instance
-- for it.
{-# OPTIONS_GHC -Wno-deprecations      #-}

module Control.Monad.IOSim.Types
  ( IOSim (..)
  , runIOSim
  , traceM
  , traceSTM
  , liftST
  , SimA (..)
  , STMSim
  , STM (..)
  , runSTM
  , StmA (..)
  , StmTxResult (..)
  , BranchStmA (..)
  , StmStack (..)
  , TimeoutException (..)
  , setCurrentTime
  , unshareClock
  , ScheduleControl (..)
  , isDefaultSchedule
  , ScheduleMod (..)
  , ExplorationOptions (..)
  , ExplorationSpec
  , withScheduleBound
  , withBranching
  , withStepTimelimit
  , withReplay
  , stdExplorationOptions
  , EventlogEvent (..)
  , EventlogMarker (..)
  , SimEventType (..)
  , ppSimEventType
  , SimEvent (..)
  , SimResult (..)
  , ppSimResult
  , SimTrace
  , Trace.Trace (SimTrace, SimPORTrace, TraceMainReturn, TraceMainException, TraceDeadlock, TraceRacesFound, TraceLoop, TraceInternalError)
  , ppTrace
  , ppTrace_
  , ppSimEvent
  , ppDebug
  , module Control.Monad.IOSim.CommonTypes
  , Thrower (..)
  , SI.Time (..)
  , SI.addTime
  , SI.diffTime
    -- * Internal API
  , Timeout (..)
  , newTimeout
  , readTimeout
  , cancelTimeout
  , awaitTimeout
    -- * Low-level API
  , execReadTVar
  ) where

import Control.Applicative
import Control.Exception (ErrorCall (..), SomeAsyncException)
import Control.Exception qualified as IO
import Control.Monad
import Control.Monad.Fix (MonadFix (..))

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict.TVar (StrictTVar)
import Control.Concurrent.Class.MonadSTM.Strict.TVar qualified as StrictTVar
import Control.Monad.Class.MonadAsync hiding (Async)
import Control.Monad.Class.MonadAsync qualified as MonadAsync
import Control.Monad.Class.MonadEventlog
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM.Internal (MonadInspectSTM (..),
           MonadLabelledSTM (..), MonadSTM, MonadTraceSTM (..), TArrayDefault,
           TChanDefault (..), TMVarDefault (..), TSemDefault (..), TraceValue,
           atomically, retry)
import Control.Monad.Class.MonadSTM.Internal qualified as MonadSTM
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow as MonadThrow hiding (getMaskingState)
import Control.Monad.Class.MonadThrow qualified as MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTime.SI qualified as SI
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadTimer.SI (TimeoutState (..))
import Control.Monad.Class.MonadTimer.SI qualified as SI
import Control.Monad.Class.MonadUnique
import Control.Monad.Primitive qualified as Prim
import Control.Monad.ST.Lazy
import Control.Monad.ST.Strict qualified as StrictST
import Control.Monad.ST.Unsafe (unsafeSTToIO)

import Control.Monad.Catch qualified as Exceptions
import Control.Monad.Fail qualified as Fail

import Data.Bifoldable
import Data.Bifunctor (bimap)
import Data.Dynamic (Dynamic, toDyn)
import Data.Hashable (Hashable (hash))
import Data.List.Trace qualified as Trace
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..))
import Data.Semigroup (Max (..))
import Data.STRef.Lazy
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Typeable
import Data.Word (Word64)
import Debug.Trace qualified as Debug
import NoThunks.Class (NoThunks (..))
import Text.Printf

import GHC.Exts (oneShot)
import GHC.Generics (Generic)
import Quiet (Quiet (..))

import Control.Monad.IOSim.CommonTypes
import Control.Monad.IOSim.CommonTypes qualified as Sim
import Control.Monad.IOSim.STM
import Control.Monad.IOSimPOR.Types


import Control.DeepSeq (force)
import Data.List (intercalate)
import GHC.IO (mkUserError, unsafePerformIO)
import System.IO.Error qualified as IO.Error (userError)

{-# ANN module "HLint: ignore Use readTVarIO" #-}
newtype IOSim s a = IOSim { unIOSim :: forall r. (a -> SimA s r) -> SimA s r }

runIOSim :: IOSim s a -> SimA s a
runIOSim (IOSim k) = k Return

-- | 'IOSim' has the ability to store any 'Typeable' value in its trace which
-- can then be recovered with `selectTraceEventsDynamic` or
-- `selectTraceEventsDynamic'`.
--
traceM :: Typeable a => a -> IOSim s ()
traceM !x = IOSim $ oneShot $ \k -> Output (toDyn x) (k ())

-- | Trace a value, in the same was as `traceM` does, but from the `STM` monad.
-- This is primarily useful for debugging.
--
traceSTM :: Typeable a => a -> STMSim s ()
traceSTM !x = STM $ oneShot $ \k -> OutputStm (toDyn x) (k ())

data Thrower = ThrowSelf | ThrowOther deriving (Ord, Eq, Show)

data SimA s a where
  Return       :: a -> SimA s a

  Say          :: String -> SimA s b -> SimA s b
  Output       :: !Dynamic -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetMonoTime  :: (SI.Time    -> SimA s b) -> SimA s b
  GetWallTime  :: (UTCTime -> SimA s b) -> SimA s b
  SetWallTime  ::  UTCTime -> SimA s b  -> SimA s b
  UnshareClock :: SimA s b -> SimA s b

  StartTimeout      :: DiffTime -> SimA s a -> (Maybe a -> SimA s b) -> SimA s b
  UnregisterTimeout :: TimeoutId -> SimA s a -> SimA s a
  RegisterDelay     :: DiffTime -> (TVar s Bool -> SimA s b) -> SimA s b
  ThreadDelay       :: DiffTime -> SimA s b -> SimA s b

  NewTimeout    :: DiffTime -> (Timeout s -> SimA s b) -> SimA s b
  CancelTimeout :: Timeout s -> SimA s b -> SimA s b

  Throw        :: SomeException -> SimA s a
  Catch        :: Exception e =>
                  SimA s a -> (e -> SimA s a) -> (a -> SimA s b) -> SimA s b
  Evaluate     :: a -> (a -> SimA s b) -> SimA s b

  Fork           :: IOSim s () -> (IOSimThreadId -> SimA s b) -> SimA s b
  GetThreadId    :: (IOSimThreadId -> SimA s b) -> SimA s b
  LabelThread    :: IOSimThreadId -> String -> SimA s b -> SimA s b
  GetThreadLabel :: IOSimThreadId -> (Maybe String -> SimA s b) -> SimA s b

  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

  ThrowTo      :: SomeException -> IOSimThreadId -> SimA s a -> SimA s a
  SetMaskState :: MaskingState  -> IOSim s a -> (a -> SimA s b) -> SimA s b
  GetMaskState :: (MaskingState -> SimA s b) -> SimA s b

  YieldSim     :: SimA s a -> SimA s a

  ExploreRaces :: SimA s b -> SimA s b

  Fix          :: (x -> IOSim s x) -> (x -> SimA s r) -> SimA s r
  NewUnique    :: (Sim.Unique s -> SimA s r) -> SimA s r


newtype STM s a = STM { unSTM :: forall r. (a -> StmA s r) -> StmA s r }

instance Semigroup a => Semigroup (STM s a) where
    a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (STM s a) where
    mempty = pure mempty

runSTM :: STM s a -> StmA s a
runSTM (STM k) = k ReturnStm

data StmA s a where
  ReturnStm    :: a -> StmA s a
  ThrowStm     :: SomeException -> StmA s a
  CatchStm     :: StmA s a -> (SomeException -> StmA s a) -> (a -> StmA s b) -> StmA s b

  NewTVar      :: (VarId -> TVarId) -> Maybe String -> x -> (TVar s x -> StmA s b) -> StmA s b
  LabelTVar    :: String -> TVar s a -> StmA s b -> StmA s b
  ReadTVar     :: TVar s a -> (a -> StmA s b) -> StmA s b
  WriteTVar    :: TVar s a ->  a -> StmA s b  -> StmA s b
  Retry        :: StmA s b
  OrElse       :: StmA s a -> StmA s a -> (a -> StmA s b) -> StmA s b

  SayStm       :: String -> StmA s b -> StmA s b
  OutputStm    :: Dynamic -> StmA s b -> StmA s b
  TraceTVar    :: forall s a b.
                  TVar s a
               -> (Maybe a -> a -> ST s TraceValue)
               -> StmA s b -> StmA s b

  LiftSTStm    :: StrictST.ST s a -> (a -> StmA s b) -> StmA s b
  FixStm       :: (x -> STM s x) -> (x -> StmA s r) -> StmA s r

-- | `IOSim`'s 'Control.Monad.Class.MonadSTM.Internal.STM' monad, as 'IOSim' it
-- is parametrised by @s@, e.g. @STMSim s a@ is monadic expression of type @a@.
--
type STMSim = STM

--
-- Monad class instances
--

instance Functor (IOSim s) where
    {-# INLINE fmap #-}
    fmap f = \d -> IOSim $ oneShot $ \k -> unIOSim d (k . f)

instance Applicative (IOSim s) where
    {-# INLINE pure #-}
    pure = \x -> IOSim $ oneShot $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> IOSim $ oneShot $ \k ->
                        unIOSim df (\f -> unIOSim dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> IOSim $ oneShot $ \k -> unIOSim dm (\_ -> unIOSim dn k)

instance Monad (IOSim s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> IOSim $ oneShot $ \k -> unIOSim dm (\m -> unIOSim (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Semigroup a => Semigroup (IOSim s a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (IOSim s a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = liftA2 mappend
#endif

instance Fail.MonadFail (IOSim s) where
  fail msg = IOSim $ oneShot $ \_ -> Throw (toException (IO.Error.userError msg))

instance MonadFix (IOSim s) where
    mfix f = IOSim $ oneShot $ \k -> Fix f k

instance Alternative (IOSim s) where
    empty = throwIO (mkUserError "mzero")
    (<|>) !a b = a `catch` \(_ :: IOError) -> b

instance MonadPlus (IOSim s)

instance Functor (STM s) where
    {-# INLINE fmap #-}
    fmap f = \d -> STM $ oneShot $ \k -> unSTM d (k . f)

instance Applicative (STM s) where
    {-# INLINE pure #-}
    pure = \x -> STM $ oneShot $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> STM $ oneShot $ \k ->
                        unSTM df (\f -> unSTM dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> STM $ oneShot $ \k -> unSTM dm (\_ -> unSTM dn k)

instance Monad (STM s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> STM $ oneShot $ \k -> unSTM dm (\m -> unSTM (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Fail.MonadFail (STM s) where
  fail msg = STM $ oneShot $ \_ -> ThrowStm (toException (ErrorCall msg))

instance Alternative (STM s) where
    empty = MonadSTM.retry
    (<|>) = MonadSTM.orElse

instance MonadPlus (STM s) where

instance MonadFix (STM s) where
    mfix f = STM $ oneShot $ \k -> FixStm f k

instance MonadSay (IOSim s) where
  say msg = IOSim $ oneShot $ \k -> Say msg (k ())

instance MonadThrow (IOSim s) where
  throwIO e = IOSim $ oneShot $ \_ -> Throw (toException e)
#if __GLASGOW_HASKELL__ >= 910
  annotateIO ann io = io `catch` \e -> throwIO (IO.addExceptionContext ann e)
#endif

instance MonadEvaluate (IOSim s) where
  evaluate a = IOSim $ oneShot $ \k -> Evaluate a k

-- | Just like the IO instance, we don't actually check anything here
instance NoThunks (IOSim s a) where
  showTypeOf _ = "IOSim"
  wNoThunks _ctxt _act = return Nothing

instance Exceptions.MonadThrow (IOSim s) where
  throwM = MonadThrow.throwIO

instance MonadThrow (STM s) where
  throwIO e = STM $ oneShot $ \_ -> ThrowStm (toException e)
#if __GLASGOW_HASKELL__ >= 910
  annotateIO ann io = io `catch` \e -> throwIO (IO.addExceptionContext ann e)
#endif

  -- Since these involve re-throwing the exception and we don't provide
  -- CatchSTM at all, then we can get away with trivial versions:
  bracket before after thing = do
    a <- before
    r <- thing a
    _ <- after a
    return r

  finally thing after = do
    r <- thing
    _ <- after
    return r

instance Exceptions.MonadThrow (STM s) where
  throwM = MonadThrow.throwIO


instance MonadCatch (STM s) where

  catch action handler = STM $ oneShot $ \k -> CatchStm (runSTM action) (runSTM . fromHandler handler) k
    where
      -- Get a total handler from the given handler
      fromHandler :: Exception e => (e -> STM s a) -> SomeException -> STM s a
      fromHandler h e = case fromException e of
        Nothing -> throwIO e  -- Rethrow the exception if handler does not handle it.
        Just e' -> h e'

  -- Masking is not required as STM actions are always run inside
  -- `execAtomically` and behave as if masked. Also note that the default
  -- implementation of `generalBracket` needs mask, and is part of `MonadThrow`.
  generalBracket acquire release use = do
    resource <- acquire
    b <- use resource `catch` \e -> do
      _ <- release resource (ExitCaseException e)
      throwIO e
    c <- release resource (ExitCaseSuccess b)
    return (b, c)

instance Exceptions.MonadCatch (STM s) where
  catch = MonadThrow.catch

instance MonadCatch (IOSim s) where
  catch action handler =
    IOSim $ oneShot $ \k -> Catch (runIOSim action) (runIOSim . handler) k

instance Exceptions.MonadCatch (IOSim s) where
  catch = MonadThrow.catch

instance MonadMask (IOSim s) where
  mask action = do
      b <- getMaskingStateImpl
      case b of
        Unmasked              -> block $ action unblock
        MaskedInterruptible   -> action block
        MaskedUninterruptible -> action blockUninterruptible

  uninterruptibleMask action = do
      b <- getMaskingStateImpl
      case b of
        Unmasked              -> blockUninterruptible $ action unblock
        MaskedInterruptible   -> blockUninterruptible $ action block
        MaskedUninterruptible -> action blockUninterruptible

  getMaskingState = getMaskingStateImpl
  interruptible action = do
      b <- getMaskingStateImpl
      case b of
        Unmasked              -> action
        MaskedInterruptible   -> unblock action
        MaskedUninterruptible -> action

instance MonadMaskingState (IOSim s)

instance Exceptions.MonadMask (IOSim s) where
  mask                = MonadThrow.mask
  uninterruptibleMask = MonadThrow.uninterruptibleMask

  generalBracket acquire release use =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (use resource) `catch` \e -> do
        _ <- release resource (Exceptions.ExitCaseException e)
        throwIO e
      c <- release resource (Exceptions.ExitCaseSuccess b)
      return (b, c)

instance NoThunks a => NoThunks (StrictTVar (IOSim s) a) where
  showTypeOf _ = "StrictTVar IOSim"
  wNoThunks ctxt tvar = do
      a <- unsafeSTToIO . lazyToStrictST . execReadTVar . StrictTVar.toLazyTVar
                        $ tvar
      noThunks ctxt a

execReadTVar :: TVar s a -> ST s a
execReadTVar TVar{tvarCurrent} = readSTRef tvarCurrent
{-# INLINE execReadTVar #-}

getMaskingStateImpl :: IOSim s MaskingState
unblock, block, blockUninterruptible :: IOSim s a -> IOSim s a

getMaskingStateImpl    = IOSim  GetMaskState
unblock              a = IOSim (SetMaskState Unmasked a)
block                a = IOSim (SetMaskState MaskedInterruptible a)
blockUninterruptible a = IOSim (SetMaskState MaskedUninterruptible a)

instance MonadThread (IOSim s) where
  type ThreadId (IOSim s) = IOSimThreadId
  myThreadId       = IOSim $ oneShot $ \k -> GetThreadId k
  labelThread t l  = IOSim $ oneShot $ \k -> LabelThread t l (k ())
  threadLabel t    = IOSim $ oneShot $ \k -> GetThreadLabel t k

instance MonadFork (IOSim s) where
  forkIO task        = IOSim $ oneShot $ \k -> Fork task k
  forkOn _ task      = IOSim $ oneShot $ \k -> Fork task k
  forkIOWithUnmask f = forkIO (f unblock)
  forkFinally task k = mask $ \restore ->
                       forkIO $ try (restore task) >>= k
  throwTo tid e      = IOSim $ oneShot $ \k -> ThrowTo (toException e) tid (k ())
  yield              = IOSim $ oneShot $ \k -> YieldSim (k ())
  getNumCapabilities = return 1

instance MonadTest (IOSim s) where
  exploreRaces       = IOSim $ oneShot $ \k -> ExploreRaces (k ())

instance MonadSay (STMSim s) where
  say msg = STM $ oneShot $ \k -> SayStm msg (k ())


instance MonadLabelledSTM (IOSim s) where
  labelTVar tvar label = STM $ \k -> LabelTVar label tvar (k ())
  labelTVarIO tvar label = IOSim $ oneShot $ \k ->
                                   LiftST ( lazyToStrictST $
                                            writeSTRef (tvarLabel tvar) $! Just label
                                          ) k
  labelTQueue  = labelTQueueDefault
  labelTBQueue = labelTBQueueDefault

instance MonadSTM (IOSim s) where
  type STM       (IOSim s) = STM s
  type TVar      (IOSim s) = TVar s
  type TMVar     (IOSim s) = TMVarDefault (IOSim s)
  type TQueue    (IOSim s) = TQueueDefault (IOSim s)
  type TBQueue   (IOSim s) = TBQueueDefault (IOSim s)
  type TArray    (IOSim s) = TArrayDefault (IOSim s)
  type TSem      (IOSim s) = TSemDefault (IOSim s)
  type TChan     (IOSim s) = TChanDefault (IOSim s)

  atomically action = IOSim $ oneShot $ \k -> Atomically action k

  newTVar         x = STM $ oneShot $ \k -> NewTVar TVarId Nothing x k
  readTVar   tvar   = STM $ oneShot $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ oneShot $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ oneShot $ \_ -> Retry
  orElse        a b = STM $ oneShot $ \k -> OrElse (runSTM a) (runSTM b) k

  newTMVar          = \a -> STM $ oneShot $ \k -> NewTVar TMVarId Nothing (Just a) (k . TMVar)
  newEmptyTMVar     = STM $ oneShot $ \k -> NewTVar TMVarId Nothing Nothing (k . TMVar)
  takeTMVar         = MonadSTM.takeTMVarDefault
  tryTakeTMVar      = MonadSTM.tryTakeTMVarDefault
  putTMVar          = MonadSTM.putTMVarDefault
  tryPutTMVar       = MonadSTM.tryPutTMVarDefault
  readTMVar         = MonadSTM.readTMVarDefault
  tryReadTMVar      = MonadSTM.tryReadTMVarDefault
  swapTMVar         = MonadSTM.swapTMVarDefault
  writeTMVar        = MonadSTM.writeTMVarDefault
  isEmptyTMVar      = MonadSTM.isEmptyTMVarDefault

  newTQueue         = STM $ oneShot $ \k -> NewTVar TQueueId Nothing ([], []) (k . TQueue)
  readTQueue        = readTQueueDefault
  tryReadTQueue     = tryReadTQueueDefault
  peekTQueue        = peekTQueueDefault
  tryPeekTQueue     = tryPeekTQueueDefault
  flushTQueue       = flushTQueueDefault
  writeTQueue       = writeTQueueDefault
  isEmptyTQueue     = isEmptyTQueueDefault
  unGetTQueue       = unGetTQueueDefault

  newTBQueue size   | size >= fromIntegral (maxBound :: Int)
                    = error "newTBQueue: size larger than Int"
                    | otherwise
                    = STM $ oneShot $ \k -> NewTVar TBQueueId Nothing ([], 0, [], size) (k . (`TBQueue` size ))
  readTBQueue       = readTBQueueDefault
  tryReadTBQueue    = tryReadTBQueueDefault
  peekTBQueue       = peekTBQueueDefault
  tryPeekTBQueue    = tryPeekTBQueueDefault
  flushTBQueue      = flushTBQueueDefault
  writeTBQueue      = writeTBQueueDefault
  lengthTBQueue     = lengthTBQueueDefault
  isEmptyTBQueue    = isEmptyTBQueueDefault
  isFullTBQueue     = isFullTBQueueDefault
  unGetTBQueue      = unGetTBQueueDefault

  newTSem           = \i -> STM $ oneShot $ \k -> NewTVar TSemId Nothing i (k . TSem)
  waitTSem          = MonadSTM.waitTSemDefault
  signalTSem        = MonadSTM.signalTSemDefault
  signalTSemN       = MonadSTM.signalTSemNDefault

  newTChan          = MonadSTM.newTChanDefault
  newBroadcastTChan = MonadSTM.newBroadcastTChanDefault
  writeTChan        = MonadSTM.writeTChanDefault
  readTChan         = MonadSTM.readTChanDefault
  tryReadTChan      = MonadSTM.tryReadTChanDefault
  peekTChan         = MonadSTM.peekTChanDefault
  tryPeekTChan      = MonadSTM.tryPeekTChanDefault
  dupTChan          = MonadSTM.dupTChanDefault
  unGetTChan        = MonadSTM.unGetTChanDefault
  isEmptyTChan      = MonadSTM.isEmptyTChanDefault
  cloneTChan        = MonadSTM.cloneTChanDefault

instance MonadInspectSTM (IOSim s) where
  type InspectMonadSTM (IOSim s) = ST s
  inspectTVar  _                 TVar { tvarCurrent }  = readSTRef tvarCurrent
  inspectTMVar _ (MonadSTM.TMVar TVar { tvarCurrent }) = readSTRef tvarCurrent

-- | This instance adds a trace when a variable was written, just after the
-- stm transaction was committed.
--
-- Traces the first value using dynamic tracing, like 'traceM' does, i.e.  with
-- 'EventDynamic'; the string is traced using 'EventSay'.
--
instance MonadTraceSTM (IOSim s) where
  traceTVar _ tvar f = STM $ \k -> TraceTVar tvar f (k ())
  traceTVarIO tvar f = IOSim $ oneShot $ \k ->
                               LiftST ( lazyToStrictST $
                                        writeSTRef (tvarTrace tvar) $! Just f
                                      ) k
  traceTQueue  = traceTQueueDefault
  traceTBQueue = traceTBQueueDefault


instance MonadMVar (IOSim s) where
  type MVar (IOSim s) = MVarDefault (IOSim s)
  newEmptyMVar = atomically $ STM $ oneShot $ \k -> NewTVar MVarId Nothing (MVarEmpty mempty mempty) (k . MVar)
  newMVar      = \a -> atomically $ STM $ oneShot $ \k -> NewTVar MVarId Nothing (MVarFull a mempty) (k . MVar)
  takeMVar     = takeMVarDefault
  putMVar      = putMVarDefault
  tryTakeMVar  = tryTakeMVarDefault
  tryPutMVar   = tryPutMVarDefault
  readMVar     = readMVarDefault
  tryReadMVar  = tryReadMVarDefault
  isEmptyMVar  = isEmptyMVarDefault

instance MonadInspectMVar (IOSim s) where
  type InspectMVarMonad (IOSim s) = ST s
  inspectMVar p (MVar tvar) = do
      st <- inspectTVar p tvar
      case st of
        MVarEmpty _ _ -> pure Nothing
        MVarFull x _  -> pure (Just x)

instance MonadTraceMVar (IOSim s) where
  traceMVarIO :: forall proxy a.
                 proxy
              -> MVar (IOSim s) a
              -> (    Maybe (Maybe a)
                  ->         Maybe a
                  -> ST s TraceValue
                 )
              -> IOSim s ()
  traceMVarIO _ (MVar mvar) f = traceTVarIO mvar f'
    where
      f' :: Maybe (MVarState m a)
         ->        MVarState m a
         -> ST s TraceValue
      f' mst st = f (g <$> mst) (g st)

      g :: MVarState m a -> Maybe a
      g MVarEmpty{}    = Nothing
      g (MVarFull a _) = Just a

instance MonadLabelledMVar (IOSim s) where
  labelMVar = labelMVarDefault

instance MonadUnique (IOSim s) where
  type Unique (IOSim s) = Sim.Unique s
  newUnique  = IOSim (oneShot NewUnique)
  hashUnique = hash

data Async s a = Async !IOSimThreadId (STM s (Either SomeException a))

instance Eq (Async s a) where
    Async tid _ == Async tid' _ = tid == tid'

instance Ord (Async s a) where
    compare (Async tid _) (Async tid' _) = compare tid tid'

instance Functor (Async s) where
  fmap f (Async tid a) = Async tid (fmap f <$> a)

instance MonadAsync (IOSim s) where
  type Async (IOSim s) = Async s

  async action = do
    var <- MonadSTM.newEmptyTMVarIO
    tid <- mask $ \restore ->
             forkIO $ try (restore action)
                  >>= MonadSTM.atomically . MonadSTM.putTMVar var
    MonadSTM.labelTMVarIO var ("async-" ++ show tid)
    return (Async tid (MonadSTM.readTMVar var))

  asyncOn _  = async
  asyncBound = async

  asyncThreadId (Async tid _) = tid

  waitCatchSTM (Async _ w) = w
  pollSTM      (Async _ w) = (Just <$> w) `MonadSTM.orElse` return Nothing

  cancel a@(Async tid _) = throwTo tid AsyncCancelled <* waitCatch a
  cancelWith a@(Async tid _) e = throwTo tid e <* waitCatch a

  asyncWithUnmask k = async (k unblock)
  asyncOnWithUnmask _ k = async (k unblock)

-- | This provides access to (almost) everything from the
-- @primitive@ package, but don't try to use the @MVar@s as that will not
-- work as expected.
--
-- @since 1.4.1.0
instance Prim.PrimMonad (IOSim s) where
  type PrimState (IOSim s) = s
  primitive st = IOSim $ oneShot $ \k -> LiftST (Prim.primitive st) k

instance MonadST (IOSim s) where
  stToIO = liftST

-- | Lift an 'StrictST.ST' computation to 'IOSim'.
--
-- Note: you can use 'MonadST' to lift 'StrictST.ST' computations, this is
-- a more convenient function just for 'IOSim'.
--
liftST :: StrictST.ST s a -> IOSim s a
liftST action = IOSim $ oneShot $ \k -> LiftST action k

instance MonadMonotonicTimeNSec (IOSim s) where
  getMonotonicTimeNSec = IOSim $ oneShot $ \k -> GetMonoTime (k . conv)
    where
      -- convert time in picoseconds to nanoseconds
      conv :: SI.Time -> Word64
      conv (SI.Time d) = fromIntegral (diffTimeToPicoseconds d `div` 1_000)

instance SI.MonadMonotonicTime (IOSim s) where
  getMonotonicTime = IOSim $ oneShot $ \k -> GetMonoTime k

instance MonadTime (IOSim s) where
  getCurrentTime   = IOSim $ oneShot $ \k -> GetWallTime k

-- | Set the current wall clock time for the thread's clock domain.
--
setCurrentTime :: UTCTime -> IOSim s ()
setCurrentTime t = IOSim $ oneShot $ \k -> SetWallTime t (k ())

-- | Put the thread into a new wall clock domain, not shared with the parent
-- thread. Changing the wall clock time in the new clock domain will not affect
-- the other clock of other threads. All threads forked by this thread from
-- this point onwards will share the new clock domain.
--
unshareClock :: IOSim s ()
unshareClock = IOSim $ oneShot $ \k -> UnshareClock (k ())

instance MonadDelay (IOSim s) where
  -- Use optimized IOSim primitive
  threadDelay d =
    IOSim $ oneShot $ \k -> ThreadDelay (SI.microsecondsAsIntToDiffTime d)
                                        (k ())

instance SI.MonadDelay (IOSim s) where
  threadDelay d =
    IOSim $ oneShot $ \k -> ThreadDelay d (k ())

data Timeout s = Timeout !(TVar s TimeoutState) !TimeoutId
               -- ^ a timeout
               | NegativeTimeout !TimeoutId
               -- ^ a negative timeout

newTimeout :: DiffTime -> IOSim s (Timeout s)
newTimeout d = IOSim $ oneShot $ \k -> NewTimeout d k

readTimeout :: Timeout s -> STM s TimeoutState
readTimeout (Timeout var _key)     = MonadSTM.readTVar var
readTimeout (NegativeTimeout _key) = pure TimeoutCancelled

cancelTimeout :: Timeout s -> IOSim s ()
cancelTimeout t = IOSim $ oneShot $ \k -> CancelTimeout t (k ())

awaitTimeout :: Timeout s -> STM s Bool
awaitTimeout t  = do s <- readTimeout t
                     case s of
                       TimeoutPending   -> retry
                       TimeoutFired     -> return True
                       TimeoutCancelled -> return False

instance MonadTimer (IOSim s) where
  timeout d action
    | d <  0 = Just <$> action
    | d == 0 = return Nothing
    | otherwise = IOSim $ oneShot $ \k -> StartTimeout d' (runIOSim action) k
        where
          d' = SI.microsecondsAsIntToDiffTime d

  registerDelay d = IOSim $ oneShot $ \k -> RegisterDelay d' k
    where
      d' = SI.microsecondsAsIntToDiffTime d

instance SI.MonadTimer (IOSim s) where
  timeout d action
    | d <  0 = Just <$> action
    | d == 0 = return Nothing
    | otherwise = IOSim $ oneShot $ \k -> StartTimeout d (runIOSim action) k

  registerDelay d = IOSim $ oneShot $ \k -> RegisterDelay d k
  registerDelayCancellable d = do
    t <- newTimeout d
    return (readTimeout t, cancelTimeout t)

newtype TimeoutException = TimeoutException TimeoutId deriving Eq

instance Show TimeoutException where
    show (TimeoutException tmid) = "<<timeout " ++ show tmid ++ " >>"

instance Exception TimeoutException where
  toException   = IO.asyncExceptionToException
  fromException = IO.asyncExceptionFromException

-- | Wrapper for Eventlog events so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogEvent = EventlogEvent String

-- | Wrapper for Eventlog markers so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogMarker = EventlogMarker String

instance MonadEventlog (IOSim s) where
  traceEventIO  = traceM . EventlogEvent
  traceMarkerIO = traceM . EventlogMarker
  flushEventLog = pure ()

-- | 'Trace' is a recursive data type, it is the trace of a 'IOSim'
-- computation.  The trace will contain information about thread scheduling,
-- blocking on 'TVar's, and other internal state changes of 'IOSim'.  More
-- importantly it also supports traces generated by the computation with 'say'
-- (which corresponds to using 'putStrLn' in 'IO'), 'traceEventM', or
-- dynamically typed traces with 'traceM' (which generalise the @base@ library
-- 'Debug.Trace.traceM')
--
-- It also contains information on discovered races.
--
-- See also: 'Control.Monad.IOSim.traceEvents',
-- 'Control.Monad.IOSim.traceResult', 'Control.Monad.IOSim.selectTraceEvents',
-- 'Control.Monad.IOSim.selectTraceEventsDynamic' and
-- 'Control.Monad.IOSim.printTraceEventsSay'.
--
data SimEvent
    -- | Used when using `IOSim`.
  = SimEvent {
      seTime        :: !SI.Time,
      seThreadId    :: !IOSimThreadId,
      seThreadLabel :: !(Maybe ThreadLabel),
      seType        :: !SimEventType
    }
    -- | Only used for /IOSimPOR/
  | SimPOREvent {
      seTime        :: !SI.Time,
      seThreadId    :: !IOSimThreadId,
      seStep        :: !Int,
      seThreadLabel :: !(Maybe ThreadLabel),
      seType        :: !SimEventType
    }
    -- | Only used for /IOSimPOR/
  | SimRacesFound [ScheduleControl]
  deriving Generic
  deriving Show via Quiet SimEvent

-- | Pretty print a 'SimEvent'.
--
ppSimEvent :: Int -- ^ width of the time
           -> Int -- ^ width of thread id
           -> Int -- ^ width of thread label
           -> SimEvent
           -> String

ppSimEvent timeWidth tidWidth tLabelWidth SimEvent {seTime = SI.Time time, seThreadId, seThreadLabel, seType} =
    printf "%-*s - %-*s %-*s - %s"
           timeWidth
           (show time)
           tidWidth
           (ppIOSimThreadId seThreadId)
           tLabelWidth
           (fromMaybe "" seThreadLabel)
           (ppSimEventType seType)

ppSimEvent timeWidth tidWidth tLableWidth SimPOREvent {seTime = SI.Time time, seThreadId, seStep, seThreadLabel, seType} =
    printf "%-*s - %-*s %-*s - %s"
           timeWidth
           (show time)
           tidWidth
           (ppStepId (seThreadId, seStep))
           tLableWidth
           (fromMaybe "" seThreadLabel)
           (ppSimEventType seType)

ppSimEvent _ _ _ (SimRacesFound controls) =
    "RacesFound "++show controls


-- | A result type of a simulation.
data SimResult a
    = MainReturn    !SI.Time !(Labelled IOSimThreadId) a ![Labelled IOSimThreadId]
    -- ^ Return value of the main thread.
    | MainException !SI.Time !(Labelled IOSimThreadId) SomeException ![Labelled IOSimThreadId]
    -- ^ Exception thrown by the main thread.
    | Deadlock      !SI.Time ![Labelled IOSimThreadId]
    -- ^ Deadlock discovered in the simulation.  Deadlocks are discovered if
    -- simply the simulation cannot do any progress in a given time slot and
    -- there's no event which would advance the time.
    | Loop
    -- ^ Only returned by /IOSimPOR/ when a step execution took longer than
    -- 'explorationStepTimelimit` was exceeded.
    | InternalError String
    -- ^ An `IOSim` bug, please report to <https://github.com/intersectmbo/io-sim>
    deriving (Show, Functor)

ppSimResult :: Show a
            => Int
            -> Int
            -> Int
            -> SimResult a
            -> String
ppSimResult timeWidth tidWidth thLabelWidth r = case r of
    MainReturn (SI.Time time) tid a tids ->
      printf "%-*s - %-*s %-*s - %s %s"
             timeWidth
             (show time)
             tidWidth
             (ppIOSimThreadId (l_labelled tid))
             thLabelWidth
             (fromMaybe "" $ l_label tid)
             ("MainReturn " ++ show a)
             ("[" ++ intercalate "," (ppLabelled ppIOSimThreadId `map` tids) ++ "]")
    MainException (SI.Time time) tid e tids ->
      printf "%-*s - %-*s %-*s - %s %s"
             timeWidth
             (show time)
             tidWidth
             (ppIOSimThreadId (l_labelled tid))
             thLabelWidth
             (fromMaybe "" $ l_label tid)
             ("MainException " ++ show e)
             ("[" ++ intercalate "," (ppLabelled ppIOSimThreadId `map` tids) ++ "]")
    Deadlock (SI.Time time) tids ->
      printf "%-*s - %-*s %-*s - %s %s"
             timeWidth
             (show time)
             tidWidth
             ""
             thLabelWidth
             ""
             "Deadlock"
             ("[" ++ intercalate "," (ppLabelled ppIOSimThreadId `map` tids) ++ "]")
    Loop -> "<<io-sim-por: step execution exceded explorationStepTimelimit>>"
    InternalError e -> "<<io-sim internal error: " ++ show e ++ ">>"


-- | A type alias for 'IOSim' simulation trace.  It comes with useful pattern
-- synonyms.
--
type SimTrace a = Trace.Trace (SimResult a) SimEvent

-- | Pretty print simulation trace.
--
-- Note: this is not a streaming function, it will evaluate the whole trace
-- before printing it.  If you need to print a very large trace, you might want
-- to use
--
-- @'Trace.ppTrace' show ('ppSimEvent' 0 0 0)@
--
ppTrace :: Show a => SimTrace a -> String
ppTrace tr = Trace.ppTrace
               (ppSimResult timeWidth tidWidth labelWidth)
               (ppSimEvent timeWidth tidWidth labelWidth)
               tr
  where
    (Max timeWidth, Max tidWidth, Max labelWidth) =
        bimaximum
      . bimap (const (Max 0, Max 0, Max 0))
              (\a -> case a of
                SimEvent {seTime = SI.Time time, seThreadId, seThreadLabel} ->
                  ( Max (length (show time))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimPOREvent {seTime = SI.Time time, seThreadId, seThreadLabel} ->
                  ( Max (length (show time))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimRacesFound {} ->
                  (Max 0, Max 0, Max 0)
              )
      $ tr


-- | Like 'ppTrace' but does not show the result value.
--
ppTrace_ :: Trace.Trace a SimEvent -> String
ppTrace_ tr = Trace.ppTrace
                (const "")
                (ppSimEvent timeWidth tidWidth labelWidth)
                tr
  where
    (Max timeWidth, Max tidWidth, Max labelWidth) =
        bimaximum
      . bimap (const (Max 0, Max 0, Max 0))
              (\a -> case a of
                SimEvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimPOREvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimRacesFound {} ->
                  (Max 0, Max 0, Max 0)
              )
      $ tr



-- | Trace each event using 'Debug.trace'; this is useful when a trace ends with
-- a pure error, e.g. an assertion.
--
ppDebug :: SimTrace a -> x -> x
ppDebug = appEndo
        . foldMap (Endo . Debug.trace . show)
        . Trace.toList


pattern SimTrace :: SI.Time -> IOSimThreadId -> Maybe ThreadLabel -> SimEventType -> SimTrace a
                 -> SimTrace a
pattern SimTrace time threadId threadLabel traceEvent trace =
    Trace.Cons (SimEvent time threadId threadLabel traceEvent)
               trace

pattern SimPORTrace :: SI.Time -> IOSimThreadId -> Int -> Maybe ThreadLabel -> SimEventType -> SimTrace a
                    -> SimTrace a
pattern SimPORTrace time threadId step threadLabel traceEvent trace =
    Trace.Cons (SimPOREvent time threadId step threadLabel traceEvent)
               trace

pattern TraceRacesFound :: [ScheduleControl] -> SimTrace a
                        -> SimTrace a
pattern TraceRacesFound controls trace =
    Trace.Cons (SimRacesFound controls)
               trace

pattern TraceMainReturn :: SI.Time -> Labelled IOSimThreadId -> a -> [Labelled IOSimThreadId]
                        -> SimTrace a
pattern TraceMainReturn time tid a threads = Trace.Nil (MainReturn time tid a threads)

pattern TraceMainException :: SI.Time -> Labelled IOSimThreadId -> SomeException -> [Labelled IOSimThreadId]
                           -> SimTrace a
pattern TraceMainException time tid err threads = Trace.Nil (MainException time tid err threads)

pattern TraceDeadlock :: SI.Time -> [Labelled IOSimThreadId]
                      -> SimTrace a
pattern TraceDeadlock time threads = Trace.Nil (Deadlock time threads)

pattern TraceLoop :: SimTrace a
pattern TraceLoop = Trace.Nil Loop

pattern TraceInternalError :: String -> SimTrace a
pattern TraceInternalError msg = Trace.Nil (InternalError msg)

{-# COMPLETE SimTrace, SimPORTrace, TraceMainReturn, TraceMainException, TraceDeadlock, TraceLoop, TraceInternalError #-}


-- | Events recorded by the simulation.
--
data SimEventType
  = EventSay  String
  -- ^ hold value of `say`
  | EventLog  Dynamic
  -- ^ hold a dynamic value of `Control.Monad.IOSim.traceM`
  | EventMask MaskingState
  -- ^ masking state changed

  | EventThrow          SomeException
  -- ^ throw exception
  | EventThrowTo        SomeException IOSimThreadId
  -- ^ throw asynchronous exception (`throwTo`)
  | EventThrowToBlocked
  -- ^ the thread which executed `throwTo` is blocked
  | EventThrowToWakeup
  -- ^ the thread which executed `throwTo` is woken up
  | EventThrowToUnmasked (Labelled IOSimThreadId)
  -- ^ a target thread of `throwTo` unmasked its exceptions, this is paired
  -- with `EventThrowToWakeup` for threads which were blocked on `throwTo`
  | EventEvaluationError SomeException
  | EventEvaluationSuccess

  | EventThreadForked    IOSimThreadId
  -- ^ forked a thread
  | EventThreadFinished
  -- ^ thread terminated normally
  | EventThreadUnhandled SomeException
  -- ^ thread terminated by an unhandled exception

  | EventUniqueCreated Integer
  -- ^ created the n-th 'Unique'

  --
  -- STM events
  --

  -- | committed STM transaction
  | EventTxCommitted   [Labelled TVarId] -- ^ stm tx wrote to these
                       [Labelled TVarId] -- ^ and created these
                       (Maybe Effect)    -- ^ effect performed (only for `IOSimPOR`)
  -- | aborted an STM transaction (by an exception)
  --
  -- For /IOSimPOR/ it also holds performed effect.
  | EventTxAborted     (Maybe Effect)
  -- | STM transaction blocked (due to `retry`)
  | EventTxBlocked     [Labelled TVarId] -- stm tx blocked reading these
                       (Maybe Effect)    -- ^ effect performed (only for `IOSimPOR`)
  | EventTxWakeup      [Labelled TVarId] -- ^ changed vars causing retry

  | EventUnblocked     [IOSimThreadId]
  -- ^ unblocked threads by a committed STM transaction

  --
  -- Timeouts, Timers & Delays
  --

  | EventThreadDelay        TimeoutId SI.Time
  -- ^ thread delayed
  | EventThreadDelayFired   TimeoutId
  -- ^ thread woken up after a delay

  | EventTimeoutCreated        TimeoutId IOSimThreadId SI.Time
  -- ^ new timeout created (via `timeout`)
  | EventTimeoutFired          TimeoutId
  -- ^ timeout fired

  | EventRegisterDelayCreated TimeoutId TVarId SI.Time
  -- ^ registered delay (via `registerDelay`)
  | EventRegisterDelayFired TimeoutId
  -- ^ registered delay fired

  | EventTimerCreated         TimeoutId TVarId SI.Time
  -- ^ a new 'Timeout' created (via `newTimeout`)
  | EventTimerCancelled       TimeoutId
  -- ^ a 'Timeout' was cancelled (via `cancelTimeout`)
  | EventTimerFired           TimeoutId
  -- ^ a 'Timeout` fired

  --
  -- threadStatus
  --

  -- | event traced when `threadStatus` is executed
  | EventThreadStatus  IOSimThreadId -- ^ current thread
                       IOSimThreadId -- ^ queried thread

  --
  -- /IOSimPOR/ events
  --

  | EventSimStart      ScheduleControl
  -- ^ /IOSimPOR/ event: new execution started exploring the given schedule.
  | EventThreadSleep
  -- ^ /IOSimPOR/ event: the labelling thread was runnable, but its execution
  -- was delayed, until 'EventThreadWake'.
  --
  -- Event inserted to mark a difference between a failed trace and a similar
  -- passing trace.
  | EventThreadWake
  -- ^ /IOSimPOR/ event: marks when the thread was rescheduled by /IOSimPOR/
  | EventDeschedule    Deschedule
  -- ^ /IOSim/ and /IOSimPOR/ event: a thread was descheduled
  | EventFollowControl        ScheduleControl
  -- ^ /IOSimPOR/ event: following given schedule
  | EventAwaitControl  StepId ScheduleControl
  -- ^ /IOSimPOR/ event: thread delayed to follow the given schedule
  | EventPerformAction StepId
  -- ^ /IOSimPOR/ event: perform action of the given step
  | EventReschedule           ScheduleControl
  -- ^ /IOSimPOR/ event: reschedule a thread following the given
  -- `ScheduleControl`

  | EventEffect VectorClock Effect
  -- ^ /IOSimPOR/ event: executed effect; Useful for debugging IOSimPOR or
  -- showing compact information about thread execution.
  | EventRaces Races
  -- ^ /IOSimPOR/ event: races.  Races are updated while we execute
  -- a simulation.  Useful for debugging IOSimPOR.
  deriving Show

unsafeEvaluateString :: String -> String -> String
unsafeEvaluateString name a = unsafePerformIO $
  catchJust
    (\e -> case fromException @SomeAsyncException e of
              Just{}  -> Nothing
              Nothing -> Just e
    )
    (evaluate (force a))
    (\(e :: SomeException) -> return ("- error evaluating " ++ name ++ ": " ++ show e))


ppSimEventType :: SimEventType -> String
ppSimEventType = \case
  EventSay a -> "Say " ++ a
  EventLog a -> "Dynamic " ++ show a
  EventMask a -> "Mask " ++ show a
  EventThrow err -> "Throw " ++ unsafeEvaluateString "exception" (show err)
  EventThrowTo err tid ->
    concat [ "ThrowTo (",
              unsafeEvaluateString "exception" (show err), ") ",
              ppIOSimThreadId tid ]
  EventThrowToBlocked -> "ThrowToBlocked"
  EventThrowToWakeup -> "ThrowToWakeup"
  EventEvaluationError err -> "EvaluationError " ++ show err
  EventEvaluationSuccess -> "EvaluationSuccess"
  EventThrowToUnmasked a ->
    "ThrowToUnmasked " ++ ppLabelled ppIOSimThreadId a
  EventThreadForked a ->
    "ThreadForked " ++ ppIOSimThreadId a
  EventThreadFinished -> "ThreadFinished"
  EventThreadUnhandled a ->
    "ThreadUnhandled " ++ show a
  EventUniqueCreated n -> "UniqueCreated " ++ show n
  EventTxCommitted written created mbEff ->
    concat [ "TxCommitted ",
             ppList (ppLabelled show) written, " ",
             ppList (ppLabelled show) created,
             maybe "" ((' ' :) . ppEffect) mbEff ]

  EventTxAborted mbEff ->
    concat [ "TxAborted",
             maybe "" ((' ' :) . ppEffect) mbEff ]
  EventTxBlocked blocked mbEff ->
   concat [ "TxBlocked ",
             ppList (ppLabelled show) blocked,
             maybe "" ((' ' :) . ppEffect) mbEff ]
  EventTxWakeup changed ->
    "TxWakeup " ++ ppList (ppLabelled show) changed
  EventUnblocked unblocked ->
    "Unblocked " ++ ppList ppIOSimThreadId unblocked
  EventThreadDelay tid t ->
    concat [ "ThreadDelay ",
             show tid, " ",
             show t ]
  EventThreadDelayFired  tid -> "ThreadDelayFired " ++ show tid
  EventTimeoutCreated timer tid t ->
    concat [ "TimeoutCreated ",
             show timer, " ",
             ppIOSimThreadId tid, " ",
             show t ]
  EventTimeoutFired timer ->
    "TimeoutFired " ++ show timer
  EventRegisterDelayCreated timer tvarId t ->
    concat [ "RegisterDelayCreated ",
             show timer, " ",
             show tvarId, " ",
             show t ]
  EventRegisterDelayFired timer -> "RegisterDelayFired " ++ show timer
  EventTimerCreated timer tvarId t ->
    concat [ "TimerCreated ",
              show timer, " ",
              show tvarId, " ",
              show t ]
  EventTimerCancelled timer -> "TimerCancelled " ++ show timer
  EventTimerFired timer -> "TimerFired " ++ show timer
  EventThreadStatus  tid tid' ->
    concat [ "ThreadStatus ",
             ppIOSimThreadId tid, " ",
             ppIOSimThreadId tid' ]
  EventSimStart a -> "SimStart " ++ show a
  EventThreadSleep -> "ThreadSleep"
  EventThreadWake -> "ThreadWake"
  EventDeschedule a -> "Deschedule " ++ show a
  EventFollowControl a -> "FollowControl " ++ show a
  EventAwaitControl s a ->
    concat [ "AwaitControl ",
             ppStepId s, " ",
             show a ]
  EventPerformAction a -> "PerformAction " ++ ppStepId a
  EventReschedule a -> "Reschedule " ++ show a
  EventEffect clock eff ->
    concat [ "Effect ",
             ppVectorClock clock, " ",
             ppEffect eff ]
  EventRaces a -> show a

--
-- Executing STM Transactions
--

-- | Result of an STM computation.
--
data StmTxResult s a =
       -- | A committed transaction reports the vars that were written (in order
       -- of first write) so that the scheduler can unblock other threads that
       -- were blocked in STM transactions that read any of these vars.
       --
       -- It reports the vars that were read, so we can update vector clocks
       -- appropriately.
       --
       -- The third list of vars is ones that were created during this
       -- transaction.  This is useful for an implementation of 'traceTVar'.
       --
       -- It also includes the updated TVarId name supply.
       --
       StmTxCommitted a ![SomeTVar s] -- ^ written tvars
                        ![SomeTVar s] -- ^ read tvars
                        ![SomeTVar s] -- ^ created tvars
                        ![Dynamic]
                        ![String]
                        !VarId -- updated TVarId name supply

       -- | A blocked transaction reports the vars that were read so that the
       -- scheduler can block the thread on those vars.
       --
     | StmTxBlocked  ![SomeTVar s]

       -- | An aborted transaction reports the vars that were read so that the
       -- vector clock can be updated.
       --
     | StmTxAborted  ![SomeTVar s] SomeException


-- | A branch indicates that an alternative statement is available in the current
-- context. For example, `OrElse` has two alternative statements, say "left"
-- and "right". While executing the left statement, `OrElseStmA` branch indicates
-- that the right branch is still available, in case the left statement fails.
data BranchStmA s a =
       -- | `OrElse` statement with its 'right' alternative.
       OrElseStmA (StmA s a)
       -- | `CatchStm` statement with the 'catch' handler.
     | CatchStmA (SomeException -> StmA s a)
       -- | Unlike the other two branches, the no-op branch is not an explicit
       -- part of the STM syntax. It simply indicates that there are no
       -- alternative statements left to be executed. For example, when running
       -- right alternative of the `OrElse` statement or when running the catch
       -- handler of a `CatchStm` statement, there are no alternative statements
       -- available. This case is represented by the no-op branch.
     | NoOpStmA

data StmStack s b a where
  -- | Executing in the context of a top level 'atomically'.
  AtomicallyFrame  :: StmStack s a a

  -- | Executing in the context of the /left/ hand side of a branch.
  -- A right branch is represented by a frame containing empty statement.
  BranchFrame      :: !(BranchStmA s a)       -- right alternative, can be empty
                   -> (a -> StmA s b)         -- subsequent continuation
                   -> Map TVarId (SomeTVar s) -- saved written vars set
                   -> [SomeTVar s]            -- saved written vars list
                   -> [SomeTVar s]            -- created vars list
                   -> StmStack s b c
                   -> StmStack s a c

---
--- Exploration options
---

-- | Race exploration options.
--
data ExplorationOptions = ExplorationOptions{
    explorationScheduleBound :: Int,
    -- ^ This is an upper bound on the number of schedules with race reversals
    -- that will be explored; a bound of zero means that the default schedule
    -- will be explored, but no others. Setting the bound to zero makes
    -- IOSimPOR behave rather like IOSim, in that only one schedule is
    -- explored, but (a) IOSimPOR is considerably slower, because it still
    -- collects information on potential races, and (b) the IOSimPOR schedule
    -- is different (based on priorities, in contrast to IOSim's round-robin),
    -- and plays better with shrinking.
    --
    -- The default value is `100`.
    explorationBranching     :: Int,
    -- ^ The branching factor. This is the number of alternative schedules that
    -- IOSimPOR tries to run, per race reversal. With the default parameters,
    -- IOSimPOR will try to reverse the first 33 (100 div 3) races discovered
    -- using the default schedule, then (if 33 or more races are discovered),
    -- for each such reversed race, will run the reversal and try to reverse
    -- two more races in the resulting schedule. A high branching factor will
    -- explore more combinations of reversing fewer races, within the overall
    -- schedule bound. A branching factor of one will explore only schedules
    -- resulting from a single race reversal (unless there are fewer races
    -- available to be reversed than the schedule bound).
    --
    -- The default value is `3`.
    explorationStepTimelimit :: Maybe Int,
    -- ^ Limit on the computation time allowed per scheduling step, for
    -- catching infinite loops etc.
    --
    -- The default value is `Nothing`.
    explorationReplay        :: Maybe ScheduleControl,
    -- ^ A schedule to replay.
    --
    -- The default value is `Nothing`.
    explorationDebugLevel    :: Int
    -- ^ Log detailed trace to stderr containing information on discovered
    -- races.  The trace does not contain the result of the simulation, unless
    -- one will do that explicitly inside the simulation.
    --
    -- level 0: don't show any output,
    -- level 1: show simulation trace with discovered schedules
    -- level 2: show simulation trace with discovered schedules and races
    --
    -- NOTE: discovered schedules & races are not exposed to the user in the
    -- callback of `exploreSimTrace` or in the output of `controlSimTrace`.
  }
  deriving Show

stdExplorationOptions :: ExplorationOptions
stdExplorationOptions = ExplorationOptions{
    explorationScheduleBound = 100,
    explorationBranching     = 3,
    explorationStepTimelimit = Nothing,
    explorationReplay        = Nothing,
    explorationDebugLevel    = 0
    }

type ExplorationSpec = ExplorationOptions -> ExplorationOptions

withScheduleBound :: Int -> ExplorationSpec
withScheduleBound n e = e{explorationScheduleBound = n}

withBranching :: Int -> ExplorationSpec
withBranching n e = e{explorationBranching = n}

withStepTimelimit :: Int -> ExplorationSpec
withStepTimelimit n e = e{explorationStepTimelimit = Just n}

withReplay :: ScheduleControl -> ExplorationSpec
withReplay r e = e{explorationReplay = Just r}
