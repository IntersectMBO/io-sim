-- | This module corresponds to `Control.Concurrent.STM.TChan` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TChan
  ( StrictTChan
  , LazyTChan
  , toLazyTChan
  , fromLazyTChan
  , castStrictTChan
  , newTChan
  , newBroadcastTChan
  , writeTChan
  , readTChan
  , tryReadTChan
  , peekTChan
  , tryPeekTChan
  , dupTChan
  , unGetTChan
  , isEmptyTChan
  , cloneTChan
    -- * MonadLabelSTM
  , labelTChan
  , labelTChanIO
  ) where


import Control.Concurrent.Class.MonadSTM.TChan qualified as Lazy
import Control.Monad.Class.MonadSTM


type LazyTChan   m = Lazy.TChan m

newtype StrictTChan m a = StrictTChan { toLazyTChan :: LazyTChan m a }

fromLazyTChan :: LazyTChan m a -> StrictTChan m a
fromLazyTChan = StrictTChan

castStrictTChan :: LazyTChan m ~ LazyTChan n
                => StrictTChan m a -> StrictTChan n a
castStrictTChan (StrictTChan var) = StrictTChan var

newTChan :: MonadSTM m => STM m (StrictTChan m a)
newTChan = StrictTChan <$> Lazy.newTChan

newBroadcastTChan :: MonadSTM m => STM m (StrictTChan m a)
newBroadcastTChan = StrictTChan <$> Lazy.newBroadcastTChan

writeTChan :: MonadSTM m => StrictTChan m a -> a -> STM m ()
writeTChan (StrictTChan chan) !a = Lazy.writeTChan chan a

readTChan :: MonadSTM m => StrictTChan m a -> STM m a
readTChan = Lazy.readTChan . toLazyTChan

tryReadTChan :: MonadSTM m => StrictTChan m a -> STM m (Maybe a)
tryReadTChan = Lazy.tryReadTChan . toLazyTChan

peekTChan :: MonadSTM m => StrictTChan m a -> STM m a
peekTChan = Lazy.peekTChan . toLazyTChan

tryPeekTChan :: MonadSTM m => StrictTChan m a -> STM m (Maybe a)
tryPeekTChan = Lazy.tryPeekTChan . toLazyTChan

dupTChan :: MonadSTM m => StrictTChan m a -> STM m (StrictTChan m a)
dupTChan = fmap fromLazyTChan . Lazy.dupTChan . toLazyTChan

unGetTChan :: MonadSTM m => StrictTChan m a -> a -> STM m ()
unGetTChan (StrictTChan chan) !a = Lazy.unGetTChan chan a

isEmptyTChan :: MonadSTM m => StrictTChan m a -> STM m Bool
isEmptyTChan = Lazy.isEmptyTChan . toLazyTChan

cloneTChan :: MonadSTM m => StrictTChan m a -> STM m (StrictTChan m a)
cloneTChan = fmap fromLazyTChan . Lazy.cloneTChan . toLazyTChan

labelTChan :: MonadLabelledSTM m => StrictTChan m a -> String -> STM m ()
labelTChan (StrictTChan chan) = Lazy.labelTChan chan

labelTChanIO :: MonadLabelledSTM m => StrictTChan m a -> String -> m ()
labelTChanIO v = atomically . labelTChan v
