{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}

#if  defined(__GLASGOW_HASKELL__) && \
    !defined(mingw32_HOST_OS) && \
    !defined(__GHCJS__) && \
    !defined(js_HOST_ARCH) && \
    !defined(wasm32_HOST_ARCH)
#define GHC_TIMERS_API
#endif

-- | Timers for 'IO' that go through a single process-wide timer service
-- rather than each registering with GHC's timer manager directly.
--
-- GHC keeps its timeouts in one 'Data.IORef.IORef' which every
-- 'GHC.Conc.threadDelay', 'GHC.Conc.registerDelay' and
-- 'System.Timeout.timeout' updates with a lazy @atomicModifyIORef'@.  Each
-- update is a thunk that the next caller has to force, and a caller
-- descheduled between publishing its thunk and forcing it leaves every later
-- caller, and the timer manager itself, blocked on its black hole until it is
-- scheduled again.  With a few hundred threads using timers at once the chains
-- run to hundreds of threads and unwind one scheduler round at a time.
--
-- Here all timers are kept in one 'Map' ordered by deadline and only two
-- threads ever talk to GHC's manager: the manager's own callback thread,
-- which fires whatever is due and arms the next deadline, and a small service
-- thread which re-arms when a deadline earlier than the armed one is
-- registered.  Clients register and cancel with one STM transaction each.
-- Timers are fired from GHC's callback, exactly where 'GHC.Conc.registerDelay'
-- fires today, so no extra wake-up is added in front of a waiting thread.
--
-- On platforms without "GHC.Event" the functions below are the ones from
-- @base@.
--
module Control.Monad.Class.MonadTimer.IO
  ( -- * Replacements for @base@
    threadDelay
  , registerDelay
  , timeout
  , Timeout
    -- * The timer service
  , Fire (..)
  , TimerKey
  , registerFire
  , cancelFire
  , pendingTimers
  ) where

#ifdef GHC_TIMERS_API

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread,
           myThreadId, rtsSupportsBoundThreads, throwTo)
import Control.Concurrent qualified as Base (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM hiding (registerDelay)
import Control.Concurrent.STM qualified as Base (registerDelay)
import Control.Exception (Exception (..), asyncExceptionFromException,
           asyncExceptionToException, bracket, bracketOnError, handleJust,
           mask_, onException, uninterruptibleMask_)
import Control.Monad (void, when)
import Data.IORef
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (labelThread)
import GHC.Event qualified as GHC
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout qualified as Base (timeout)


-- | Monotonic time in nanoseconds, as 'getMonotonicTimeNSec' reports it.
type Deadline = Word64

-- | What to do when a timer fires.  'fireSTM' runs inside the transaction that
-- fires all the due timers, 'fireIO' after it.  Both run on GHC's timer manager
-- thread or on the service thread and so must neither block nor throw.
--
-- A timer that was cancelled after it fell due may still fire, because
-- cancellations are applied when the service next drains its inbox; the fire
-- actions must tolerate that.  Those below do.
--
data Fire = Fire {
    fireSTM :: STM (),
    fireIO  :: IO ()
  }

-- | Identifies a registered timer, for 'cancelFire'.  The 'TVar' is only an
-- identity.
--
data TimerKey = TimerKey !Deadline !(TVar ())

-- | What clients hand to the service.
data Op = Register !Deadline !(TVar ()) Fire
        | Cancel   !Deadline !(TVar ())

data Service = Service {
    -- | registrations and cancellations not yet applied, newest first; the
    -- only state clients write
    svInbox   :: !(TVar [Op]),
    -- | length of 'svInbox', to bound the work of a drain
    svInboxN  :: !(TVar Int),
    -- | pending timers by deadline; owned by whoever holds 'svArmed'
    svPending :: !(IORef (Map Deadline [(TVar (), Fire)])),
    -- | the deadline GHC's timeout is armed for; read by 'registerFire' to
    -- decide whether the service thread has to re-arm
    svArmedAt :: !(TVar (Maybe Deadline)),
    -- | the armed GHC timeout; the 'MVar' also serialises the callback, the
    -- service thread and 'pendingTimers'
    svArmed   :: !(MVar (Maybe GHC.TimeoutKey)),
    -- | set when the service thread has to drain and re-arm
    svWake    :: !(TVar Bool)
  }

-- | Drain the inbox at this many operations even if none needs re-arming.
inboxLimit :: Int
inboxLimit = 4096

-- | The service needs "GHC.Event", which only the threaded RTS runs.  The
-- replacements for @base@ below fall back to @base@ without it; the service
-- itself does not.
--
service :: Service
service = unsafePerformIO $ do
    when (not rtsSupportsBoundThreads) $
      error "Control.Monad.Class.MonadTimer.IO: the timer service requires the threaded RTS"
    sv <- Service <$> newTVarIO []
                  <*> newTVarIO 0
                  <*> newIORef Map.empty
                  <*> newTVarIO Nothing
                  <*> newMVar Nothing
                  <*> newTVarIO False
    tid <- forkIO (serviceThread sv)
    labelThread tid "io-classes timer service"
    return sv
{-# NOINLINE service #-}


-- | Register a timer to fire @us@ microseconds from now.  Negative delays
-- fire at once.  One STM transaction on the inbox.
--
registerFire :: Int -> Fire -> IO TimerKey
registerFire us fire = do
    now <- getMonotonicTimeNSec
    let deadline = deadlineAfter now us
    ident <- newTVarIO ()
    earlier <- atomically $ do
      n <- post service (Register deadline ident fire)
      armedAt <- readTVar (svArmedAt service)
      when (n >= inboxLimit) $ writeTVar (svWake service) True
      return (maybe True (deadline <) armedAt)
    when earlier $ rearmNow service
    return (TimerKey deadline ident)

-- | A deadline earlier than the armed one needs GHC's timeout moved.  Do it
-- from the calling thread when the lock is free, so that no other thread has
-- to be scheduled first; when it is held, whoever holds it is arming and the
-- service thread covers the case that it missed this registration.  Never
-- blocks, so it is safe under 'mask_'.
--
rearmNow :: Service -> IO ()
rearmNow sv = mask_ $ do
    held <- tryTakeMVar (svArmed sv)
    case held of
      Nothing    -> atomically $ writeTVar (svWake sv) True
      Just armed -> do
        armed' <- arm sv armed `onException` putMVar (svArmed sv) armed
        putMVar (svArmed sv) armed'

-- | Cancel a timer.  Cancelling one that already fired is a no-op.
--
cancelFire :: TimerKey -> IO ()
cancelFire (TimerKey deadline ident) =
    atomically $ do
      n <- post service (Cancel deadline ident)
      when (n >= inboxLimit) $ writeTVar (svWake service) True

-- | Append to the inbox; returns its new length.
post :: Service -> Op -> STM Int
post sv op = do
    modifyTVar' (svInbox sv) (op :)
    n <- readTVar (svInboxN sv)
    writeTVar (svInboxN sv) $! n + 1
    return (n + 1)

-- | The number of timers waiting to fire; for tests.
--
pendingTimers :: IO Int
pendingTimers = withMVar (svArmed service) $ \_ -> do
    drain service
    sum . map length . Map.elems <$> readIORef (svPending service)


deadlineAfter :: Deadline -> Int -> Deadline
deadlineAfter now us
  | us <= 0                  = now
  | us > maxBound `div` 1000 = maxBound
  | ns > maxBound - now      = maxBound
  | otherwise                = now + ns
  where
    ns = fromIntegral us * 1000

-- | Microseconds from now until the deadline, zero if it has passed.
--
microsecondsUntil :: Deadline -> Deadline -> Int
microsecondsUntil now deadline
  | deadline <= now = 0
  | otherwise       = fromIntegral ((deadline - now) `div` 1000)


-- | Apply the inbox to the pending map, oldest operation first.  Called with
-- the 'svArmed' lock held.
--
drain :: Service -> IO ()
drain sv = do
    ops <- atomically $ do
      ops <- readTVar (svInbox sv)
      writeTVar (svInbox sv) []
      writeTVar (svInboxN sv) 0
      return ops
    modifyIORef' (svPending sv) $ \pending -> foldl' apply pending (reverse ops)
  where
    apply pending (Register d ident fire) =
      Map.insertWith (++) d [(ident, fire)] pending
    apply pending (Cancel d ident) =
      Map.update (nonEmpty . filter ((/= ident) . fst)) d pending

    nonEmpty [] = Nothing
    nonEmpty es = Just es

-- | Drain and re-arm when asked to: when the inbox has grown long, or when
-- a registration found the lock taken.
--
serviceThread :: Service -> IO ()
serviceThread sv = go
  where
    go = do
      atomically $ do
        wake <- readTVar (svWake sv)
        check wake
        writeTVar (svWake sv) False
      modifyMVar_ (svArmed sv) (arm sv)
      go

-- | GHC's callback: fire what is due, then arm for the next deadline.  The
-- timeout that called us has fired, so the armed key is spent whatever
-- 'svArmed' says.
--
callback :: Service -> IO ()
callback sv = modifyMVar_ (svArmed sv) $ \_spent -> do
    fireDue sv
    arm sv Nothing

-- | Fire every pending timer whose deadline has passed.  Called with the
-- 'svArmed' lock held.
--
fireDue :: Service -> IO ()
fireDue sv = do
    drain sv
    now <- getMonotonicTimeNSec
    pending <- readIORef (svPending sv)
    let (due, rest) = Map.spanAntitone (<= now) pending
        fires = map snd (concat (Map.elems due))
    writeIORef (svPending sv) rest
    atomically (mapM_ fireSTM fires)
    mapM_ fireIO fires

-- | Make GHC's timeout match the earliest pending deadline, firing any that
-- is already due first.  Called with the 'svArmed' lock held.
--
-- GHC runs the callback of a timeout registered with a non-positive delay
-- inline, which here would re-enter the lock, so a deadline that has passed
-- is fired directly and GHC only ever sees delays of at least a microsecond.
--
-- A registration that lands between the drain and the record of the new
-- armed deadline compares itself with the previous one.  If it is earlier
-- than that it wakes the service thread or arms itself; if it is later than
-- that but earlier than the deadline armed here, which happens when the
-- previous earliest timer was cancelled, it would be missed, so the inbox is
-- looked at once more after arming and the whole step repeats if the
-- earliest deadline moved.
--
arm :: Service -> Maybe GHC.TimeoutKey -> IO (Maybe GHC.TimeoutKey)
arm sv armed = do
    drain sv
    now <- getMonotonicTimeNSec
    next <- earliest
    atomically $ writeTVar (svArmedAt sv) next
    mgr <- GHC.getSystemTimerManager
    case (next, armed) of
      (Just d, _) | d <= now -> do
        fireDue sv
        arm sv armed
      (Nothing, Nothing)  -> settle Nothing next
      (Nothing, Just key) -> do
        GHC.unregisterTimeout mgr key
        settle Nothing next
      (Just d, Nothing)   -> do
        key <- GHC.registerTimeout mgr (delayUntil now d) (callback sv)
        settle (Just key) next
      (Just d, Just key)  -> do
        -- 'updateTimeout' is a no-op on a key that has already fired; its
        -- callback is then about to run and arms afresh.
        GHC.updateTimeout mgr key (delayUntil now d)
        settle (Just key) next
  where
    delayUntil now d = max 1 (microsecondsUntil now d)

    earliest = fmap fst . Map.lookupMin <$> readIORef (svPending sv)

    settle armed' next = do
      drain sv
      next' <- earliest
      if next' == next then return armed' else arm sv armed'


--
-- Replacements for base
--

-- | 'GHC.Conc.registerDelay' through the timer service.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay us
  | not rtsSupportsBoundThreads = Base.registerDelay us
  | otherwise = do
    v <- newTVarIO False
    _ <- registerFire us (Fire (writeTVar v True) (return ()))
    return v

-- | 'Control.Concurrent.threadDelay' through the timer service.
--
threadDelay :: Int -> IO ()
threadDelay us
  | not rtsSupportsBoundThreads = Base.threadDelay us
  | us <= 0   = return ()
  | otherwise = do
      v <- newTVarIO False
      bracketOnError (registerFire us (Fire (writeTVar v True) (return ())))
                     cancelFire
                     (\_ -> atomically (readTVar v >>= check))

-- | The exception 'timeout' interrupts its action with.  The 'IORef' is only
-- an identity, so that nested timeouts tell their exceptions apart.
--
newtype Timeout = Timeout (IORef ()) deriving Eq

instance Show Timeout where
  show _ = "<<timeout>>"

instance Exception Timeout where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | 'System.Timeout.timeout' through the timer service.  The algorithm is
-- @base@'s: the timer's action forks a thread to deliver the exception, and
-- a 'MVar' decides between that thread and the action completing.
--
timeout :: Int -> IO a -> IO (Maybe a)
timeout us action
  | not rtsSupportsBoundThreads = Base.timeout us action
  | us <  0   = Just <$> action
  | us == 0   = return Nothing
  | otherwise = do
      pid  <- myThreadId
      ex   <- Timeout <$> newIORef ()
      lock <- newEmptyMVar :: IO (MVar ThreadId)
      let handleTimeout = do
            v <- isEmptyMVar lock
            when v $ void $ forkIOWithUnmask $ \unmask -> unmask $ do
              v2 <- tryPutMVar lock =<< myThreadId
              when v2 $ throwTo pid ex
          cleanupTimeout key = uninterruptibleMask_ $ do
            v <- tryPutMVar lock undefined
            if v then cancelFire key
                 else takeMVar lock >>= killThread
      handleJust (\e -> if e == ex then Just () else Nothing)
                 (\_ -> return Nothing)
                 (bracket (mask_ (registerFire us (Fire (return ()) handleTimeout)))
                          cleanupTimeout
                          (\_ -> Just <$> action))

#else

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, registerDelay)
import System.Timeout (Timeout, timeout)

data Fire = Fire {
    fireSTM :: STM (),
    fireIO  :: IO ()
  }

data TimerKey = TimerKey

registerFire :: Int -> Fire -> IO TimerKey
registerFire _ _ = return TimerKey

cancelFire :: TimerKey -> IO ()
cancelFire _ = return ()

pendingTimers :: IO Int
pendingTimers = return 0

#endif
