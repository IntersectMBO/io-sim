{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadIORef where

import Control.Monad.Reader
import Data.IORef qualified as IO
import Data.Kind

class Monad m => MonadIORef m where
  {-# MINIMAL newIORef, readIORef, writeIORef, atomicModifyIORef, atomicModifyIORef', atomicWriteIORef #-}

  type IORef m :: Type -> Type

  -- | See 'IO.newIORef'.
  newIORef :: a -> m (IORef m a)
  -- | See 'IO.readIORef'.
  readIORef :: IORef m a -> m a
  -- | See 'IO.writeIORef'.
  writeIORef :: IORef m a -> a -> m ()
  -- | See 'IO.modifyIORef'.
  modifyIORef :: IORef m a -> (a -> a) -> m ()
  -- | See 'IO.modifyRef''.
  modifyIORef' :: IORef m a -> (a -> a) -> m ()
  -- | See 'IO.atomicModifyIORef'.
  atomicModifyIORef :: IORef m a -> (a -> (a, b)) -> m b
  -- | See 'IO.atomicModifyIORef''.
  atomicModifyIORef' :: IORef m a -> (a -> (a, b)) -> m b
  -- | See 'IO.atomicWriteIORef'.
  atomicWriteIORef :: IORef m a -> a -> m ()

  default modifyIORef :: IORef m a -> (a -> a) -> m ()
  modifyIORef ref f = readIORef ref >>= writeIORef ref . f

  default modifyIORef' :: IORef m a -> (a -> a) -> m ()
  modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

--
-- IO instance
--

instance MonadIORef IO where
  type IORef IO = IO.IORef
  newIORef = IO.newIORef
  readIORef = IO.readIORef
  writeIORef = IO.writeIORef
  modifyIORef = IO.modifyIORef
  modifyIORef' = IO.modifyIORef'
  atomicModifyIORef = IO.atomicModifyIORef
  atomicModifyIORef' = IO.atomicModifyIORef'
  atomicWriteIORef = IO.atomicWriteIORef

--
-- ReaderT instance
--

instance MonadIORef m => MonadIORef (ReaderT r m) where
  type IORef (ReaderT r m) = IORef m
  newIORef = lift . newIORef
  readIORef = lift . readIORef
  writeIORef = lift .: writeIORef
  modifyIORef = lift .: modifyIORef
  modifyIORef' = lift .: modifyIORef'
  atomicModifyIORef = lift .: atomicModifyIORef
  atomicModifyIORef' = lift .: atomicModifyIORef'
  atomicWriteIORef = lift .: atomicWriteIORef

--
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
