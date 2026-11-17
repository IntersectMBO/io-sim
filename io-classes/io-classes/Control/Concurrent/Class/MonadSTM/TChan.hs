{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrent.STM.TChan` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TChan
  ( -- * MonadSTM
    -- ** TChans
    type TChan
    -- * Construction
  , newTChan
  , newBroadcastTChan
  , newTChanIO
  , newBroadcastTChanIO
  , dupTChan
  , cloneTChan
    -- ** Reading and writing
  , readTChan
  , tryReadTChan
  , peekTChan
  , tryPeekTChan
  , writeTChan
  , unGetTChan
  , isEmptyTChan
    -- * MonadLabelledSTM
  , labelTChan
  , labelTChanIO
  ) where

import Control.Monad.Class.MonadSTM.Internal
