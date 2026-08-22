{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.IOSim.IORef where

import Control.Monad.Class.MonadIORef
import Control.Monad.Class.MonadST
import Control.Monad.IOSim.Types
import GHC.Exts
import GHC.ST
import GHC.STRef

newtype IOSimRef s a = IORef (STRef s a)

instance MonadIORef (IOSim s) where
  type IORef (IOSim s) = IOSimRef s
  newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)
  readIORef (IORef v) = stToIO (readSTRef v)
  writeIORef (IORef var) v = stToIO (writeSTRef var v)
  modifyIORef ref f = readIORef ref >>= writeIORef ref . f
  modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'
  atomicModifyIORef ref f = do
    (_old, (_new, res)) <- atomicModifyIORef2 ref f
    pure res
  atomicModifyIORef' = Control.Monad.IOSim.IORef.atomicModifyIORef'
  atomicWriteIORef ref a = do
    _ <- atomicSwapIORef ref a
    pure ()

atomicModifyIORef2Lazy :: IORef (IOSim s) a -> (a -> (a,b)) -> IOSim s (a, (a, b))
atomicModifyIORef2Lazy (IORef (STRef r#)) f = stToIO $
  ST (\s -> case atomicModifyMutVar2# r# f s of
              (# s', old, res #) -> (# s', (old, res) #))

atomicModifyIORef2 :: IORef (IOSim s) a -> (a -> (a,b)) -> IOSim s (a, (a, b))
atomicModifyIORef2 ref f = do
  r@(_old, (_new, _res)) <- atomicModifyIORef2Lazy ref f
  return r

atomicModifyIORefP :: IORef (IOSim s) a -> (a -> (a,b)) -> IOSim s b
atomicModifyIORefP ref f = do
  (_old, (_,r)) <- atomicModifyIORef2 ref f
  pure r

atomicModifyIORefLazy_ :: IORef (IOSim s) a -> (a -> a) -> IOSim s (a, a)
atomicModifyIORefLazy_ (IORef (STRef ref)) f = stToIO $ ST $ \s ->
  case atomicModifyMutVar_# ref f s of
    (# s', old, new #) -> (# s', (old, new) #)

atomicModifyIORef'_ :: IORef (IOSim s) a -> (a -> a) -> IOSim s (a, a)
atomicModifyIORef'_ ref f = do
  (old, !new) <- atomicModifyIORefLazy_ ref f
  return (old, new)

atomicSwapIORef :: IORef (IOSim s) a -> a -> IOSim s a
atomicSwapIORef (IORef (STRef ref)) new = stToIO $ ST (atomicSwapMutVar# ref new)

atomicModifyIORef' :: IORef (IOSim s) a -> (a -> (a,b)) -> IOSim s b
atomicModifyIORef' ref f = do
  (_old, (_new, !res)) <- atomicModifyIORef2 ref $
    \old -> case f old of
       r@(!_new, _res) -> r
  pure res
