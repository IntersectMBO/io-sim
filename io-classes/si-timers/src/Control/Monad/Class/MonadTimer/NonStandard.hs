{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

#if  defined(__GLASGOW_HASKELL__) && \
    !defined(mingw32_HOST_OS) && \
    !defined(__GHCJS__) && \
    !defined(js_HOST_ARCH) && \
    !defined(wasm32_HOST_ARCH)
#define GHC_TIMERS_API
#endif

-- | A non-standard interface for timer api.
--
-- This module also provides a polyfill which allows to use timer api also on
-- non-threaded RTS regardless of the architecture \/ OS.  Currently we support
-- `*nix`, `macOS`, `Windows` (and, unofficially `GHCJS`).
--
-- We use it to provide @'Control.Monad.Class.MonadTimer.MonadTimer' 'IO'@
-- instance and to implement a cancellable timers, see
-- 'Control.Monad.Class.MonadTimer.SI.registerDelayCancellable'.
--
-- You can expect we will deprecate it at some point (e.g. once GHC gets
-- a better support for timers especially across different execution
-- environments).
--
module Control.Monad.Class.MonadTimer.NonStandard
  ( TimeoutState (..)
  , newTimeout
  , readTimeout
  , cancelTimeout
  , awaitTimeout
  , NewTimeout
  , ReadTimeout
  , CancelTimeout
  , AwaitTimeout
  ) where

import Control.Concurrent.STM qualified as STM
#ifndef GHC_TIMERS_API
import Control.Monad (when)
#endif
import Control.Monad.Class.MonadSTM

#ifdef GHC_TIMERS_API
import Control.Monad.Class.MonadTimer.IO (Fire (..), TimerKey, cancelFire,
           registerFire)
#else
import GHC.Conc.IO qualified as GHC (registerDelay)
#endif


-- | State of a timeout: pending, fired or cancelled.
--
data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled
  deriving (Eq, Ord, Show)


-- | The type of the timeout handle, used with 'newTimeout', 'readTimeout', and
-- 'cancelTimeout'.
--
#ifdef GHC_TIMERS_API
data Timeout = TimeoutIO !(STM.TVar TimeoutState) !TimerKey
#else
data Timeout = TimeoutIO !(STM.TVar (STM.TVar Bool)) !(STM.TVar Bool)
#endif

-- | Create a new timeout which will fire at the given time duration in
-- the future.
--
-- The timeout will start in the 'TimeoutPending' state and either
-- fire at or after the given time leaving it in the 'TimeoutFired' state,
-- or it may be cancelled with 'cancelTimeout', leaving it in the
-- 'TimeoutCancelled' state.
--
-- Timeouts /cannot/ be reset to the pending state once fired or cancelled
-- (as this would be very racy). You should create a new timeout if you need
-- this functionality.
--
-- When native timer manager is supported (on `*nix` systems), it only holds
-- a `TVar` with `TimeoutState` and the timer service's `TimerKey`.
--
newTimeout :: NewTimeout IO Timeout
type NewTimeout m timeout = Int -> m timeout


-- | Read the current state of a timeout. This does not block, but returns
-- the current state. It is your responsibility to use 'retry' to wait.
--
-- Alternatively you may wish to use the convenience utility 'awaitTimeout'
-- to wait for just the fired or cancelled outcomes.
--
-- You should consider the cancelled state if you plan to use 'cancelTimeout'.
--
readTimeout :: ReadTimeout IO Timeout
type ReadTimeout m timeout = timeout -> STM m TimeoutState


-- | Cancel a timeout (unless it has already fired), putting it into the
-- 'TimeoutCancelled' state. Code reading and acting on the timeout state
-- need to handle such cancellation appropriately.
--
-- It is safe to race this concurrently against the timer firing. It will
-- have no effect if the timer fires first.
--
cancelTimeout :: CancelTimeout IO Timeout
type CancelTimeout m timeout = timeout -> m ()

-- | Returns @True@ when the timeout is fired, or @False@ if it is cancelled.
awaitTimeout :: AwaitTimeout IO Timeout
type AwaitTimeout m timeout = timeout -> STM m Bool


#ifdef GHC_TIMERS_API

readTimeout (TimeoutIO var _key) = STM.readTVar var

newTimeout = \d -> do
    var <- STM.newTVarIO TimeoutPending
    key <- registerFire d (Fire (timeoutAction var) (return ()))
    return (TimeoutIO var key)
  where
    timeoutAction var = do
      x <- STM.readTVar var
      case x of
        TimeoutPending   -> STM.writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(IO): invariant violation"
        TimeoutCancelled -> return ()

cancelTimeout (TimeoutIO var key) = do
    STM.atomically $ do
      x <- STM.readTVar var
      case x of
        TimeoutPending   -> STM.writeTVar var TimeoutCancelled
        TimeoutFired     -> return ()
        TimeoutCancelled -> return ()
    cancelFire key

#else

readTimeout (TimeoutIO timeoutvarvar cancelvar) = do
  canceled <- STM.readTVar cancelvar
  fired    <- STM.readTVar =<< STM.readTVar timeoutvarvar
  case (canceled, fired) of
    (True, _)  -> return TimeoutCancelled
    (_, False) -> return TimeoutPending
    (_, True)  -> return TimeoutFired

newTimeout d = do
  timeoutvar    <- GHC.registerDelay d
  timeoutvarvar <- STM.newTVarIO timeoutvar
  cancelvar     <- STM.newTVarIO False
  return (TimeoutIO timeoutvarvar cancelvar)

cancelTimeout (TimeoutIO timeoutvarvar cancelvar) =
  STM.atomically $ do
    fired <- STM.readTVar =<< STM.readTVar timeoutvarvar
    when (not fired) $ STM.writeTVar cancelvar True

#endif

awaitTimeout t  = do s <- readTimeout t
                     case s of
                       TimeoutPending   -> retry
                       TimeoutFired     -> return True
                       TimeoutCancelled -> return False
