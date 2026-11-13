{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- MonadAsync's ReaderT instance is undecidable.
{-# LANGUAGE UndecidableInstances   #-}

-- | <https://hackage.haskell.org/package/async async> API compatible with both
-- 'IO' and <https://hackage.haskell.org/package/io-sim IOSim>.
--
module Control.Monad.Class.MonadAsync
  ( MonadAsync (..)
  , AsyncCancelled (..)
  , ExceptionInLinkedThread (..)
  , link
  , linkOnly
  , link2
  , link2Only
  , mapConcurrently
  , forConcurrently
  , mapConcurrently_
  , forConcurrently_
  , replicateConcurrently
  , replicateConcurrently_
  , Concurrently (..)
  ) where

import Prelude hiding (read)

#if MIN_VERSION_base(4,18,0)
import Control.Applicative (Alternative (..))
#else
import Control.Applicative (Alternative (..), liftA2)
#endif
import Control.Monad (forever)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)

import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Concurrent.Async qualified as Async
import Control.Exception qualified as E

import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Kind (Type)

class ( MonadSTM m
      , MonadThread m
      ) => MonadAsync m where

  {-# MINIMAL async, asyncBound, asyncOn, asyncThreadId, cancel, cancelWith,
              asyncWithUnmask, asyncOnWithUnmask, waitCatchSTM, pollSTM,
              asyncLabelled, asyncLabelledBound, asyncLabelledOn,
              asyncLabelledWithUnmask, asyncLabelledOnWithUnmask #-}

  -- | An asynchronous action
  --
  -- See 'Async.Async'.
  type Async m          = (async :: Type -> Type) | async -> m

  -- | See 'Async.async'.
  async                 :: m a -> m (Async m a)
  -- | See 'Async.asyncBound'.
  asyncBound            :: m a -> m (Async m a)
  -- | See 'Async.asyncOn'.
  asyncOn               :: Int -> m a -> m (Async m a)
  -- | See 'Async.asyncThreadId'.
  asyncThreadId         :: Async m a -> ThreadId m
  -- | See 'Async.withAsync'.
  withAsync             :: m a -> (Async m a -> m b) -> m b
  -- | See 'Async.withAsyncBound'.
  withAsyncBound        :: m a -> (Async m a -> m b) -> m b
  -- | See 'Async.withAsyncOn'.
  withAsyncOn           :: Int -> m a -> (Async m a -> m b) -> m b

  -- | See 'Async.waitSTM'.
  waitSTM               :: Async m a -> STM m a
  -- | See 'Async.pollSTM'.
  pollSTM               :: Async m a -> STM m (Maybe (Either SomeException a))
  -- | See 'Async.waitCatchSTM'.
  waitCatchSTM          :: Async m a -> STM m (Either SomeException a)

  default waitSTM :: MonadThrow (STM m) => Async m a -> STM m a
  waitSTM action = waitCatchSTM action >>= either throwSTM return

  -- | See 'Async.waitAnySTM'.
  waitAnySTM            :: [Async m a] -> STM m (Async m a, a)
  -- | See 'Async.waitAnyCatchSTM'.
  waitAnyCatchSTM       :: [Async m a] -> STM m (Async m a, Either SomeException a)
  -- | See 'Async.waitEitherSTM'.
  waitEitherSTM         :: Async m a -> Async m b -> STM m (Either a b)
  -- | See 'Async.waitEitherSTM_'.
  waitEitherSTM_        :: Async m a -> Async m b -> STM m ()
  -- | See 'Async.waitEitherCatchSTM'.
  waitEitherCatchSTM    :: Async m a -> Async m b
                        -> STM m (Either (Either SomeException a)
                                         (Either SomeException b))
  -- | See 'Async.waitBothSTM'.
  waitBothSTM           :: Async m a -> Async m b -> STM m (a, b)

  -- | See 'Async.wait'.
  wait                  :: Async m a -> m a
  -- | See 'Async.poll'.
  poll                  :: Async m a -> m (Maybe (Either SomeException a))
  -- | See 'Async.waitCatch'.
  waitCatch             :: Async m a -> m (Either SomeException a)
  -- | See 'Async.cancel'.
  cancel                :: Async m a -> m ()
  -- | See 'Async.cancelWith'.
  cancelWith            :: Exception e => Async m a -> e -> m ()
  -- | See 'Async.uninterruptibleCancel'.
  uninterruptibleCancel :: Async m a -> m ()

  -- | See 'Async.waitAny'.
  waitAny               :: [Async m a] -> m (Async m a, a)
  -- | See 'Async.waitAnyCatch'.
  waitAnyCatch          :: [Async m a] -> m (Async m a, Either SomeException a)
  -- | See 'Async.waitAnyCancel'.
  waitAnyCancel         :: [Async m a] -> m (Async m a, a)
  -- | See 'Async.waitAnyCatchCancel'.
  waitAnyCatchCancel    :: [Async m a] -> m (Async m a, Either SomeException a)
  -- | See 'Async.waitEither'.
  waitEither            :: Async m a -> Async m b -> m (Either a b)

  default waitAnySTM     :: MonadThrow (STM m) => [Async m a] -> STM m (Async m a, a)
  default waitEitherSTM  :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (Either a b)
  default waitEitherSTM_ :: MonadThrow (STM m) => Async m a -> Async m b -> STM m ()
  default waitBothSTM    :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (a, b)

  waitAnySTM as =
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) as

  waitAnyCatchSTM as =
    foldr orElse retry $
      map (\a -> do r <- waitCatchSTM a; return (a, r)) as

  waitEitherSTM left right =
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)

  waitEitherSTM_ left right =
      (void $ waitSTM left)
        `orElse`
      (void $ waitSTM right)

  waitEitherCatchSTM left right =
      (Left  <$> waitCatchSTM left)
        `orElse`
      (Right <$> waitCatchSTM right)

  waitBothSTM left right = do
      a <- waitSTM left
             `orElse`
           (waitSTM right >> retry)
      b <- waitSTM right
      return (a,b)

  -- | Note, IO-based implementations should override the default
  -- implementation. See the @async@ package implementation and comments.
  -- <http://hackage.haskell.org/package/async-2.2.1/docs/src/Control.Concurrent.Async.html#waitEitherCatch>
  --
  -- See 'Async.waitEitherCatch'.
  waitEitherCatch       :: Async m a -> Async m b -> m (Either (Either SomeException a)
                                                               (Either SomeException b))
  -- | See 'Async.waitEitherCancel'.
  waitEitherCancel      :: Async m a -> Async m b -> m (Either a b)
  -- | See 'Async.waitEitherCatchCancel'.
  waitEitherCatchCancel :: Async m a -> Async m b -> m (Either (Either SomeException a)
                                                               (Either SomeException b))
  -- | See 'Async.waitEither_'.
  waitEither_           :: Async m a -> Async m b -> m ()
  -- | See 'Async.waitBoth'.
  waitBoth              :: Async m a -> Async m b -> m (a, b)

  -- | See 'Async.race'.
  race                  :: m a -> m b -> m (Either a b)
  -- | See 'Async.race_'.
  race_                 :: m a -> m b -> m ()
  -- | See 'Async.concurrently'.
  concurrently          :: m a -> m b -> m (a,b)
  -- | See 'Async.concurrently_'.
  concurrently_         :: m a -> m b -> m ()

  -- | See 'Async.concurrently_'.
  asyncWithUnmask       :: ((forall b . m b -> m b) ->  m a) -> m (Async m a)
  -- | See 'Async.asyncOnWithUnmask'.
  asyncOnWithUnmask     :: Int -> ((forall b . m b -> m b) ->  m a) -> m (Async m a)
  -- | See 'Async.withAsyncWithUnmask'.
  withAsyncWithUnmask   :: ((forall c. m c -> m c) ->  m a) -> (Async m a -> m b) -> m b
  -- | See 'Async.withAsyncOnWithUnmask'.
  withAsyncOnWithUnmask :: Int -> ((forall c. m c -> m c) ->  m a) -> (Async m a -> m b) -> m b

  -- | See 'Async.compareAsyncs'.
  compareAsyncs         :: Async m a -> Async m b -> Ordering

  -- non-standard APIs

  asyncLabelled                 :: String -> m a -> m (Async m a)
  asyncLabelledBound            :: String -> m a -> m (Async m a)
  asyncLabelledOn               :: String -> Int -> m a -> m (Async m a)
  withAsyncLabelled             :: String -> m a -> (Async m a -> m b) -> m b
  withAsyncLabelledBound        :: String -> m a -> (Async m a -> m b) -> m b
  withAsyncLabelledOn           :: String -> Int -> m a -> (Async m a -> m b) -> m b
  asyncLabelledWithUnmask       :: String -> ((forall b . m b -> m b) ->  m a) -> m (Async m a)
  asyncLabelledOnWithUnmask     :: String -> Int -> ((forall b . m b -> m b) ->  m a) -> m (Async m a)
  withAsyncLabelledWithUnmask   :: String -> ((forall c. m c -> m c) ->  m a) -> (Async m a -> m b) -> m b
  withAsyncLabelledOnWithUnmask :: String -> Int -> ((forall c. m c -> m c) ->  m a) -> (Async m a -> m b) -> m b

  -- default implementations
  default withAsync     :: MonadMask m => m a -> (Async m a -> m b) -> m b
  default withAsyncBound:: MonadMask m => m a -> (Async m a -> m b) -> m b
  default withAsyncOn   :: MonadMask m => Int -> m a -> (Async m a -> m b) -> m b
  default withAsyncWithUnmask
                        :: MonadMask m => ((forall c. m c -> m c) ->  m a)
                                       -> (Async m a -> m b) -> m b
  default withAsyncOnWithUnmask
                        :: MonadMask m => Int
                                       -> ((forall c. m c -> m c) ->  m a)
                                       -> (Async m a -> m b) -> m b
  default uninterruptibleCancel
                        :: MonadMask m => Async m a -> m ()
  default waitAnyCancel         :: MonadThrow m => [Async m a] -> m (Async m a, a)
  default waitAnyCatchCancel    :: MonadThrow m => [Async m a]
                                -> m (Async m a, Either SomeException a)
  default waitEitherCancel      :: MonadThrow m => Async m a -> Async m b
                                -> m (Either a b)
  default waitEitherCatchCancel :: MonadThrow m => Async m a -> Async m b
                                -> m (Either (Either SomeException a)
                                             (Either SomeException b))
  default compareAsyncs         :: Ord (ThreadId m)
                                => Async m a -> Async m b -> Ordering

  withAsync action =
    withAsyncUsing (\restore -> async (restore action))

  withAsyncBound action =
    withAsyncUsing (\restore -> asyncBound (restore action))

  withAsyncOn n action =
    withAsyncUsing (\restore -> asyncOn n (restore action))

  -- the child runs in the masking state of the parent, as in the `async`
  -- package; `asyncWithUnmask` forks it masked, since we are inside `mask`.
  withAsyncWithUnmask action =
    withAsyncUsing (\restore -> asyncWithUnmask (\unmask -> restore (action unmask)))

  withAsyncOnWithUnmask n action =
    withAsyncUsing (\restore -> asyncOnWithUnmask n (\unmask -> restore (action unmask)))

  wait      = atomically . waitSTM
  poll      = atomically . pollSTM
  waitCatch = atomically . waitCatchSTM

  uninterruptibleCancel      = uninterruptibleMask_ . cancel

  waitAny                    = atomically . waitAnySTM
  waitAnyCatch               = atomically . waitAnyCatchSTM
  waitEither      left right = atomically (waitEitherSTM left right)
  waitEither_     left right = atomically (waitEitherSTM_ left right)
  waitEitherCatch left right = atomically (waitEitherCatchSTM left right)
  waitBoth        left right = atomically (waitBothSTM left right)

  waitAnyCancel asyncs =
    waitAny asyncs `finally` mapM_ cancel asyncs

  waitAnyCatchCancel asyncs =
    waitAnyCatch asyncs `finally` mapM_ cancel asyncs

  waitEitherCancel left right =
    waitEither left right `finally` (cancel left >> cancel right)

  waitEitherCatchCancel left right =
    waitEitherCatch left right `finally` (cancel left >> cancel right)

  race            left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither a b

  race_           left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither_ a b

  concurrently    left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitBoth a b

  concurrently_   left right = void $ concurrently left right

  compareAsyncs a b = asyncThreadId a `compare` asyncThreadId b

  default withAsyncLabelled     :: MonadMask m => String -> m a -> (Async m a -> m b) -> m b
  default withAsyncLabelledBound:: MonadMask m => String -> m a -> (Async m a -> m b) -> m b
  default withAsyncLabelledOn   :: MonadMask m => String -> Int -> m a -> (Async m a -> m b) -> m b
  default withAsyncLabelledWithUnmask
                        :: MonadMask m => String
                                       -> ((forall c. m c -> m c) ->  m a)
                                       -> (Async m a -> m b) -> m b
  default withAsyncLabelledOnWithUnmask
                        :: MonadMask m => String
                                       -> Int
                                       -> ((forall c. m c -> m c) ->  m a)
                                       -> (Async m a -> m b) -> m b

  withAsyncLabelled lbl action =
    withAsyncUsing (\restore -> asyncLabelled lbl (restore action))

  withAsyncLabelledBound lbl action =
    withAsyncUsing (\restore -> asyncLabelledBound lbl (restore action))

  withAsyncLabelledOn lbl n action =
    withAsyncUsing (\restore -> asyncLabelledOn lbl n (restore action))

  withAsyncLabelledWithUnmask lbl action =
    withAsyncUsing (\restore -> asyncLabelledWithUnmask lbl (\unmask -> restore (action unmask)))

  withAsyncLabelledOnWithUnmask lbl n action =
    withAsyncUsing (\restore -> asyncLabelledOnWithUnmask lbl n (\unmask -> restore (action unmask)))

-- | The bracket shared by the default `withAsync` family implementations.
--
-- The first argument spawns the child thread; it runs inside `mask` and
-- receives its `restore`, which it must use to restore the parent's masking
-- state in the child.  The inner action runs with the parent's masking state,
-- and the child is cancelled when it returns or throws.
--
withAsyncUsing :: (MonadAsync m, MonadMask m)
               => ((forall x. m x -> m x) -> m (Async m a))
               -> (Async m a -> m b)
               -> m b
withAsyncUsing spawn inner = mask $ \restore -> do
    a <- spawn restore
    restore (inner a)
      `finally` uninterruptibleCancel a

-- | Similar to 'Async.Concurrently' but which works for any 'MonadAsync'
-- instance.
--
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

instance Functor m => Functor (Concurrently m) where
    fmap f (Concurrently ma) = Concurrently (fmap f ma)

instance MonadAsync m => Applicative (Concurrently m) where
    pure = Concurrently . pure

    Concurrently fn <*> Concurrently as =
      Concurrently $
        (\(f, a) -> f a)
        `fmap`
        concurrently fn as

instance ( MonadAsync  m
         , MonadTimer  m
         ) => Alternative (Concurrently m) where
    empty = Concurrently $ forever (threadDelay 86400)
    Concurrently as <|> Concurrently bs =
      Concurrently $ either id id <$> as `race` bs

instance ( Semigroup  a
         , MonadAsync m
         ) => Semigroup (Concurrently m a) where
    (<>) = liftA2 (<>)

instance ( Monoid a
         , MonadAsync m
         ) => Monoid (Concurrently m a) where
    mempty = pure mempty


-- | See 'Async.mapConcurrently'.
mapConcurrently :: (Traversable t, MonadAsync m) => (a -> m b) -> t a -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | See 'Async.forConcurrently'.
forConcurrently :: (Traversable t, MonadAsync m) => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently

-- | See 'Async.mapConcurrently_'.
mapConcurrently_ :: (Foldable f, MonadAsync m) => (a -> m b) -> f a -> m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)

-- | See 'Async.forConcurrently_'.
forConcurrently_ :: (Foldable f, MonadAsync m) => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_

-- | See 'Async.replicateConcurrently'.
replicateConcurrently :: MonadAsync m => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently . sequenceA . replicate cnt . Concurrently

-- | See 'Async.replicateConcurrently_'.
replicateConcurrently_ :: MonadAsync m => Int -> m a -> m ()
replicateConcurrently_ cnt = runConcurrently . fold . replicate cnt . Concurrently . void


--
-- Instance for IO uses the existing async library implementations
--

instance MonadAsync IO where

  type Async IO         = Async.Async

  async                 = Async.async
  asyncBound            = Async.asyncBound
  asyncOn               = Async.asyncOn
  asyncThreadId         = Async.asyncThreadId
  withAsync             = Async.withAsync
  withAsyncBound        = Async.withAsyncBound
  withAsyncOn           = Async.withAsyncOn

  waitSTM               = Async.waitSTM
  pollSTM               = Async.pollSTM
  waitCatchSTM          = Async.waitCatchSTM

  waitAnySTM            = Async.waitAnySTM
  waitAnyCatchSTM       = Async.waitAnyCatchSTM
  waitEitherSTM         = Async.waitEitherSTM
  waitEitherSTM_        = Async.waitEitherSTM_
  waitEitherCatchSTM    = Async.waitEitherCatchSTM
  waitBothSTM           = Async.waitBothSTM

  wait                  = Async.wait
  poll                  = Async.poll
  waitCatch             = Async.waitCatch
  cancel                = Async.cancel
  cancelWith            = Async.cancelWith
  uninterruptibleCancel = Async.uninterruptibleCancel

  waitAny               = Async.waitAny
  waitAnyCatch          = Async.waitAnyCatch
  waitAnyCancel         = Async.waitAnyCancel
  waitAnyCatchCancel    = Async.waitAnyCatchCancel
  waitEither            = Async.waitEither
  waitEitherCatch       = Async.waitEitherCatch
  waitEitherCancel      = Async.waitEitherCancel
  waitEitherCatchCancel = Async.waitEitherCatchCancel
  waitEither_           = Async.waitEither_
  waitBoth              = Async.waitBoth

  race                  = Async.race
  race_                 = Async.race_
  concurrently          = Async.concurrently
  concurrently_         = Async.concurrently_

  asyncWithUnmask       = Async.asyncWithUnmask
  asyncOnWithUnmask     = Async.asyncOnWithUnmask
  withAsyncWithUnmask   = Async.withAsyncWithUnmask
  withAsyncOnWithUnmask = Async.withAsyncOnWithUnmask

  compareAsyncs         = Async.compareAsyncs

  asyncLabelled lbl      = Async.async      . (labelThisThread lbl >>)
  asyncLabelledBound lbl = Async.asyncBound . (labelThisThread lbl >>)
  asyncLabelledOn lbl n  = Async.asyncOn n  . (labelThisThread lbl >>)
  asyncLabelledWithUnmask lbl k
                         = Async.asyncWithUnmask
                             (\unmask -> labelThisThread lbl >> k unmask)
  asyncLabelledOnWithUnmask lbl n k
                         = Async.asyncOnWithUnmask n
                             (\unmask -> labelThisThread lbl >> k unmask)

  withAsyncLabelled lbl      = Async.withAsync      . (labelThisThread lbl >>)
  withAsyncLabelledBound lbl = Async.withAsyncBound . (labelThisThread lbl >>)
  withAsyncLabelledOn lbl n  = Async.withAsyncOn n  . (labelThisThread lbl >>)
  withAsyncLabelledWithUnmask lbl k
                             = Async.withAsyncWithUnmask
                                 (\unmask -> labelThisThread lbl >> k unmask)
  withAsyncLabelledOnWithUnmask lbl n k
                             = Async.withAsyncOnWithUnmask n
                                 (\unmask -> labelThisThread lbl >> k unmask)


--
-- Linking
--
-- Adapted from "Control.Concurrent.Async"
--
-- We don't use the implementation of linking from 'Control.Concurrent.Async'
-- directly because  if we /did/ use the real implementation, then the mock
-- implementation and the real implementation would not be able to throw the
-- same exception, because the exception type used by the real implementation
-- is
--
-- > data ExceptionInLinkedThread =
-- >   forall a . ExceptionInLinkedThread (Async a) SomeException
--
--    containing a reference to the real 'Async' type.
--

-- | Exception from child thread re-raised in parent thread
--
-- We record the thread ID of the child thread as a 'String'. This avoids
-- an @m@ parameter in the type, which is important: 'ExceptionInLinkedThread'
-- must be an instance of 'Exception', requiring it to be 'Typeable'; if @m@
-- appeared in the type, we would require @m@ to be 'Typeable', which does not
-- work with with the simulator, as it would require a 'Typeable' constraint
-- on the @s@ parameter of 'IOSim'.
data ExceptionInLinkedThread = ExceptionInLinkedThread String SomeException

instance Show ExceptionInLinkedThread where
  showsPrec p (ExceptionInLinkedThread a e) =
    showParen (p >= 11) $
      showString "ExceptionInLinkedThread " .
      showsPrec 11 a .
      showString " " .
      showsPrec 11 e

instance Exception ExceptionInLinkedThread where
  fromException = E.asyncExceptionFromException
  toException = E.asyncExceptionToException

-- | Like 'Async.link'.
link :: (MonadAsync m, MonadFork m, MonadMask m)
     => Async m a -> m ()
link = linkOnly (not . isCancel)

-- | Like 'Async.linkOnly'.
linkOnly :: forall m a. (MonadAsync m, MonadFork m, MonadMask m)
         => (SomeException -> Bool) -> Async m a -> m ()
linkOnly shouldThrow a = do
    tid <- myThreadId
    void $ forkRepeat ("linkToOnly " <> show linkedThreadId) $ do
      r <- waitCatch a
      case r of
        Left e | shouldThrow e -> throwTo tid (exceptionInLinkedThread e)
        _otherwise             -> return ()
  where
    linkedThreadId :: ThreadId m
    linkedThreadId = asyncThreadId a

    exceptionInLinkedThread :: SomeException -> ExceptionInLinkedThread
    exceptionInLinkedThread =
        ExceptionInLinkedThread (show linkedThreadId)

-- | Like 'Async.link2'.
link2 :: (MonadAsync m, MonadFork m, MonadMask m)
      => Async m a -> Async m b -> m ()
link2 = link2Only (not . isCancel)

-- | Like 'Async.link2Only'.
link2Only :: (MonadAsync m, MonadFork m, MonadMask m)
          => (SomeException -> Bool) -> Async m a -> Async m b -> m ()
link2Only shouldThrow left  right =
  void $ forkRepeat ("link2Only " <> show (tl, tr)) $ do
    r <- waitEitherCatch left right
    case r of
      Left  (Left e) | shouldThrow e ->
        throwTo tr (ExceptionInLinkedThread (show tl) e)
      Right (Left e) | shouldThrow e ->
        throwTo tl (ExceptionInLinkedThread (show tr) e)
      _ -> return ()
  where
    tl = asyncThreadId left
    tr = asyncThreadId right

isCancel :: SomeException -> Bool
isCancel e
  | Just AsyncCancelled <- fromException e = True
  | otherwise = False

forkRepeat :: (MonadFork m, MonadMask m) => String -> m a -> m (ThreadId m)
forkRepeat label action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkLabelledIO label go

tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll = try


--
-- ReaderT instance
--

newtype AsyncReaderT r (m :: Type -> Type) a =
    AsyncReaderT { getAsyncReaderT :: Async m a }

instance ( MonadAsync m
         , MonadCatch (STM m)
         , MonadFork m
         , MonadMask m
         ) => MonadAsync (ReaderT r m) where
    type Async (ReaderT r m) = AsyncReaderT r m
    asyncThreadId (AsyncReaderT a) = asyncThreadId a

    async      (ReaderT ma)  = ReaderT $ \r -> AsyncReaderT <$> async (ma r)
    asyncBound (ReaderT ma)  = ReaderT $ \r -> AsyncReaderT <$> asyncBound (ma r)
    asyncOn n  (ReaderT ma)  = ReaderT $ \r -> AsyncReaderT <$> asyncOn n (ma r)
    withAsync (ReaderT ma) f = ReaderT $ \r -> withAsync (ma r)
                                       $ \a -> runReaderT (f (AsyncReaderT a)) r
    withAsyncBound (ReaderT ma) f = ReaderT $ \r -> withAsyncBound (ma r)
                                       $ \a -> runReaderT (f (AsyncReaderT a)) r
    withAsyncOn  n (ReaderT ma) f = ReaderT $ \r -> withAsyncOn n (ma r)
                                       $ \a -> runReaderT (f (AsyncReaderT a)) r

    asyncWithUnmask f        = ReaderT $ \r -> fmap AsyncReaderT
                                             $ asyncWithUnmask
                                             $ \unmask -> runReaderT (f (liftF unmask)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    asyncOnWithUnmask n f   = ReaderT $ \r -> fmap AsyncReaderT
                                            $ asyncOnWithUnmask n
                                            $ \unmask -> runReaderT (f (liftF unmask)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    withAsyncWithUnmask action f  =
      ReaderT $ \r -> withAsyncWithUnmask (\unmask -> case action (liftF unmask) of
                                                        ReaderT ma -> ma r)
              $ \a -> runReaderT (f (AsyncReaderT a)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    withAsyncOnWithUnmask n action f  =
      ReaderT $ \r -> withAsyncOnWithUnmask n (\unmask -> case action (liftF unmask) of
                                                            ReaderT ma -> ma r)
              $ \a -> runReaderT (f (AsyncReaderT a)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    waitCatchSTM = lift . waitCatchSTM . getAsyncReaderT
    pollSTM      = lift . pollSTM      . getAsyncReaderT

    race         (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> race  (ma r) (mb r)
    race_        (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> race_ (ma r) (mb r)
    concurrently (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> concurrently (ma r) (mb r)

    wait                  = lift .  wait         . getAsyncReaderT
    poll                  = lift .  poll         . getAsyncReaderT
    waitCatch             = lift .  waitCatch    . getAsyncReaderT
    cancel                = lift .  cancel       . getAsyncReaderT
    uninterruptibleCancel = lift .  uninterruptibleCancel
                                                 . getAsyncReaderT
    cancelWith            = (lift .: cancelWith)
                          . getAsyncReaderT
    waitAny               = fmap (first AsyncReaderT)
                          . lift . waitAny
                          . map getAsyncReaderT
    waitAnyCatch          = fmap (first AsyncReaderT)
                          . lift . waitAnyCatch
                          . map getAsyncReaderT
    waitAnyCancel         = fmap (first AsyncReaderT)
                          . lift . waitAnyCancel
                          . map getAsyncReaderT
    waitAnyCatchCancel    = fmap (first AsyncReaderT)
                          . lift . waitAnyCatchCancel
                          . map getAsyncReaderT
    waitEither            = on (lift .: waitEither)            getAsyncReaderT
    waitEitherCatch       = on (lift .: waitEitherCatch)       getAsyncReaderT
    waitEitherCancel      = on (lift .: waitEitherCancel)      getAsyncReaderT
    waitEitherCatchCancel = on (lift .: waitEitherCatchCancel) getAsyncReaderT
    waitEither_           = on (lift .: waitEither_)           getAsyncReaderT
    waitBoth              = on (lift .: waitBoth)              getAsyncReaderT

    asyncLabelled      lbl   (ReaderT ma) = ReaderT $ \r -> AsyncReaderT <$> asyncLabelled lbl (ma r)
    asyncLabelledBound lbl   (ReaderT ma) = ReaderT $ \r -> AsyncReaderT <$> asyncLabelledBound lbl (ma r)
    asyncLabelledOn    lbl n (ReaderT ma) = ReaderT $ \r -> AsyncReaderT <$> asyncLabelledOn lbl n (ma r)

    asyncLabelledWithUnmask lbl f = ReaderT $ \r -> fmap AsyncReaderT
                                                  $ asyncLabelledWithUnmask lbl
                                                  $ \unmask -> runReaderT (f (liftF unmask)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    asyncLabelledOnWithUnmask lbl n f = ReaderT $ \r -> fmap AsyncReaderT
                                                      $ asyncLabelledOnWithUnmask lbl n
                                                      $ \unmask -> runReaderT (f (liftF unmask)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    withAsyncLabelled lbl (ReaderT ma) f  = ReaderT $ \r -> withAsyncLabelled lbl (ma r)
                                                    $ \a -> runReaderT (f (AsyncReaderT a)) r
    withAsyncLabelledBound lbl (ReaderT ma) f
                                          = ReaderT $ \r -> withAsyncLabelledBound lbl (ma r)
                                                    $ \a -> runReaderT (f (AsyncReaderT a)) r
    withAsyncLabelledOn lbl n (ReaderT ma) f
                                          = ReaderT $ \r -> withAsyncLabelledOn lbl n (ma r)
                                                    $ \a -> runReaderT (f (AsyncReaderT a)) r

    withAsyncLabelledWithUnmask lbl action f =
      ReaderT $ \r -> withAsyncLabelledWithUnmask lbl (\unmask -> case action (liftF unmask) of
                                                                    ReaderT ma -> ma r)
              $ \a -> runReaderT (f (AsyncReaderT a)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)

    withAsyncLabelledOnWithUnmask lbl n action f =
      ReaderT $ \r -> withAsyncLabelledOnWithUnmask lbl n (\unmask -> case action (liftF unmask) of
                                                                        ReaderT ma -> ma r)
              $ \a -> runReaderT (f (AsyncReaderT a)) r
      where
        liftF :: (m a -> m a) -> ReaderT r m a -> ReaderT r m a
        liftF g (ReaderT r) = ReaderT (g . r)


--
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)


-- | A higher order version of 'Data.Function.on'
--
on :: (f a -> f b -> c)
   -> (forall x. g x -> f x)
   -> (g a -> g b -> c)
on f g = \a b -> f (g a) (g b)
