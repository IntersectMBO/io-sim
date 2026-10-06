{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Properties of the 'IO' timers in "Control.Monad.Class.MonadTimer.IO".
-- The oracle is the monotonic clock and arithmetic on the generated delays,
-- never the timers themselves.
--
module Test.MonadTimer.IO (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, replicateM, replicateM_, void)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout qualified as Base (timeout)

import Control.Monad.Class.MonadTimer.IO qualified as T

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- | Run in order: the properties share one timer service and some of them
-- count its pending timers or measure its latency.
tests :: TestTree
tests =
    sequentialTestGroup "Control.Monad.Class.MonadTimer.IO" AllFinish
    [ testProperty "threadDelay elapsed time"      prop_threadDelay_elapsed
    , testProperty "registerDelay flips in time"   prop_registerDelay_flips
    , testProperty "timeout result"                prop_timeout_result
    , testProperty "nested timeouts"               prop_timeout_nested
    , testProperty "cancelled timers leave no residue" prop_cancel_clean
    , testProperty "concurrent timers fire exactly once" prop_concurrent_fire_once
    , testProperty "delay under churn"             prop_delay_under_churn
    ]

-- | A delay in microseconds, up to 50 ms.
newtype Delay = Delay Int deriving (Show, Eq)

instance Arbitrary Delay where
  arbitrary = Delay <$> choose (0, 50_000)
  shrink (Delay d) = [Delay d' | d' <- shrink d, d' >= 0]

-- | How late a timer may fire and still pass.  Generous, so that a loaded
-- machine does not fail the suite, yet far below the seconds a lost re-arm
-- costs.
slack :: Int
slack = 100_000

-- | Run a service call under @base@'s own 'System.Timeout.timeout', which does
-- not go through the service, so that a dead service fails a property instead
-- of hanging the suite.
bounded :: IO a -> IO a
bounded act = do
  r <- Base.timeout 3_000_000 act
  case r of
    Just a  -> return a
    Nothing -> error "timer service did not fire within 3 s"

elapsedSince :: Word64 -> IO Int
elapsedSince t0 = do
  t1 <- getMonotonicTimeNSec
  return (fromIntegral ((t1 - t0) `div` 1000))

-- | A long timer armed first, so that the delay under test is always earlier
-- than the armed deadline and the service has to re-arm for it.
withFarTimer :: IO a -> IO a
withFarTimer act = do
  k <- T.registerFire 5_000_000 (T.Fire (return ()) (return ()))
  r <- act
  T.cancelFire k
  return r

prop_threadDelay_elapsed :: Delay -> Property
prop_threadDelay_elapsed (Delay d) = withMaxSuccess 30 $ ioProperty $ withFarTimer $ do
  t0 <- getMonotonicTimeNSec
  bounded (T.threadDelay d)
  e <- elapsedSince t0
  return $ counterexample ("elapsed " ++ show e ++ " us for " ++ show d)
         $ e >= d && e <= d + slack

prop_registerDelay_flips :: Delay -> Property
prop_registerDelay_flips (Delay d) = withMaxSuccess 30 $ ioProperty $ withFarTimer $ do
  t0 <- getMonotonicTimeNSec
  v <- T.registerDelay d
  early <- if d > 5_000 then readTVarIO v else return False
  bounded (atomically (readTVar v >>= check))
  e <- elapsedSince t0
  return $ counterexample ("flipped at " ++ show e ++ " us for " ++ show d ++ ", early " ++ show early)
         $ not early && e >= d && e <= d + slack

-- | Limit and action length far enough apart that the outcome is decided.
data Race = Race Int Int deriving (Show, Eq)

instance Arbitrary Race where
  arbitrary = do
    l <- choose (1_000, 60_000)
    a <- choose (0, 120_000) `suchThat` \a -> abs (a - l) >= 20_000
    return (Race l a)
  shrink (Race l a) = [Race l' a' | (l', a') <- shrink (l, a), l' >= 1_000, a' >= 0, abs (a' - l') >= 20_000]

prop_timeout_result :: Race -> Property
prop_timeout_result (Race l a) = withMaxSuccess 30 $ ioProperty $ do
  r <- try (T.timeout l (threadDelay a >> return a))
  return $ case r of
    Left (e :: SomeException) -> counterexample ("escaped: " ++ show e) False
    Right res -> counterexample (show res) $ res === (if a < l then Just a else Nothing)

-- | Outer and inner limits and an action, all decided.
data Nested = Nested Int Int Int deriving (Show, Eq)

instance Arbitrary Nested where
  arbitrary = do
    (o, i, a) <- (,,) <$> choose (1_000, 90_000) <*> choose (1_000, 90_000) <*> choose (1_000, 90_000)
    if abs (o - i) >= 20_000 && abs (o - a) >= 20_000 && abs (i - a) >= 20_000
      then return (Nested o i a)
      else arbitrary
  shrink _ = []

prop_timeout_nested :: Nested -> Property
prop_timeout_nested (Nested o i a) = withMaxSuccess 30 $ ioProperty $ do
  r <- try (T.timeout o (T.timeout i (threadDelay a >> return ())))
  let expected
        | a < i && a < o = Just (Just ())
        | i < o          = Just Nothing   -- the inner limit fires first
        | otherwise      = Nothing        -- the outer limit fires first
  return $ case r of
    Left (e :: SomeException) -> counterexample ("escaped: " ++ show e) False
    Right res -> counterexample (show res) $ res === expected

prop_cancel_clean :: Positive (Small Int) -> Property
prop_cancel_clean (Positive (Small n)) = withMaxSuccess 20 $ ioProperty $ do
  before <- T.pendingTimers
  -- timeouts whose actions finish at once
  replicateM_ n (T.timeout 1_000_000 (return ()))
  -- delays interrupted by an asynchronous exception
  tids <- replicateM n (forkIO (T.threadDelay 10_000_000))
  threadDelay 2_000
  mapM_ killThread tids
  threadDelay 20_000
  afterwards <- T.pendingTimers
  return $ counterexample ("pending before " ++ show before ++ ", after " ++ show afterwards)
         $ afterwards <= before

prop_concurrent_fire_once :: Positive (Small Int) -> Positive (Small Int) -> Property
prop_concurrent_fire_once (Positive (Small threads)) (Positive (Small perThread)) =
  withMaxSuccess 20 $ ioProperty $ do
    counter <- newTVarIO (0 :: Int)
    done <- newTVarIO (0 :: Int)
    forM_ [1 .. threads] $ \_ -> forkIO $ do
      forM_ [1 .. perThread] $ \j -> void $
        T.registerFire (j * 1_000) (T.Fire (modifyTVar' counter (+ 1)) (return ()))
      atomically (modifyTVar' done (+ 1))
    atomically (readTVar done >>= check . (== threads))
    threadDelay (perThread * 1_000 + slack)
    fired <- readTVarIO counter
    return $ counterexample ("fired " ++ show fired)
           $ fired === threads * perThread

-- | A 20 ms delay measured while other threads register and cancel timers
-- at about 20k operations per second each.  The oracle is @base@'s own
-- 'threadDelay' under the same churn, so that scheduling noise counts against
-- both sides alike.
prop_delay_under_churn :: Positive (Small Int) -> Property
prop_delay_under_churn (Positive (Small churners)) = withMaxSuccess 10 $ ioProperty $ do
  stop <- newTVarIO False
  -- bounded and paced: a bug in the service must not turn this into a
  -- memory bomb or a CPU-saturating spin
  tids <- forM [1 .. churners] $ \_ -> forkIO $
    let loop :: Int -> IO ()
        loop 0 = return ()
        loop i = do
          s <- readTVarIO stop
          if s then return ()
               else do k <- T.registerFire 1_000_000 (T.Fire (return ()) (return ()))
                       T.cancelFire k
                       threadDelay 50
                       loop (i - 1)
    in loop 20_000
  t0 <- getMonotonicTimeNSec
  threadDelay 20_000
  eBase <- elapsedSince t0
  t1 <- getMonotonicTimeNSec
  bounded (T.threadDelay 20_000)
  e <- elapsedSince t1
  atomically (writeTVar stop True)
  mapM_ killThread tids
  return $ counterexample ("elapsed " ++ show e ++ " us, base " ++ show eBase ++ " us")
         $ e >= 20_000 && e <= eBase + slack
