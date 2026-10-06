{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Provides classes to handle delays and timeouts which generalised
-- <https://hackage.haskell.org/package/base base> API to both 'IO' and
-- <https://hackage.haskell.org/package/io-sim IOSim>.
--
module Control.Monad.Class.MonadTimer
  ( MonadDelay (..)
  , MonadTimer (..)
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadTimer.IO qualified as IO

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)

-- | A typeclass to delay current thread.
class Monad m => MonadDelay m where

  -- | Suspends the current thread for a given number of microseconds
  -- (GHC only).
  --
  -- See `IO.threadDelay`.
  threadDelay :: Int -> m ()

-- | A typeclass providing utilities for /timeouts/.
class (MonadDelay m, MonadSTM m) => MonadTimer m where

  -- | See `STM.registerDelay`.
  registerDelay :: Int -> m (TVar m Bool)

  -- | See `IO.timeout`.
  timeout :: Int -> m a -> m (Maybe a)

--
-- Instances for IO
--
-- These go through the process-wide timer service of
-- "Control.Monad.Class.MonadTimer.IO"; see there for why.
--

instance MonadDelay IO where
  threadDelay = IO.threadDelay


instance MonadTimer IO where

  registerDelay = IO.registerDelay
  timeout = IO.timeout

--
-- Transformer's instances
--

instance MonadDelay m => MonadDelay (ReaderT r m) where
  threadDelay = lift . threadDelay

instance MonadTimer m => MonadTimer (ReaderT r m) where
  registerDelay = lift . registerDelay
  timeout d f   = ReaderT $ \r -> timeout d (runReaderT f r)
