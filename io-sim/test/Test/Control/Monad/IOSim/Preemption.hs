{-# LANGUAGE NumericUnderscores #-}

-- | Properties of the preemption oracle ('runSimTraceWith').
module Test.Control.Monad.IOSim.Preemption (tests) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad (forM, forM_, replicateM_)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup "Preemption"
  [ testProperty "window opens iff the oracle preempts long enough" prop_window
  , testProperty "preemptionsOf replays the run"                    prop_replay
  , testProperty "transactions stay atomic"                         prop_atomic
  , testProperty "a masked thread is not interrupted while preempted" prop_masked
  , testProperty "an interrupted preemption leaves no timer behind"  prop_interrupted
  ]

-- | A random oracle: seed, probability (permille) and maximal delay (µs).
data Spec = Spec Int Int Int
  deriving Show

instance Arbitrary Spec where
  arbitrary = Spec <$> arbitrary <*> choose (0, 1000) <*> choose (1, 10_000)
  shrink (Spec s p m) =
       [ Spec s p' m | p' <- shrink p, p' >= 0 ]
    ++ [ Spec s p m' | m' <- shrink m, m' >= 1 ]

oracle :: Spec -> Preemption
oracle (Spec s p m) = preemptRandom s (fromIntegral p / 1000) (micros m)

micros :: Int -> DiffTime
micros n = fromIntegral n / 1_000_000

-- | The writer commits 1 then 2; the reader reads at @d@ (half a microsecond
-- off the oracle's grid, so no tie).  It sees 1 iff the writer was preempted
-- at its first yield point (after the first commit) for longer than @d@.
prop_window :: Spec -> NonNegative Int -> Property
prop_window spec (NonNegative dUs) =
    let d = micros dUs + 0.000_000_5
        (seen, writer) = runSimOrThrowWith (oracle spec) $ do
          v <- newTVarIO (0 :: Int)
          r <- newEmptyTMVarIO
          w <- forkIO $ atomically (writeTVar v 1) >> atomically (writeTVar v 2)
          _ <- forkIO $ threadDelay d >> readTVarIO v >>= atomically . putTMVar r
          x <- atomically (takeTMVar r)
          return (x, w)
        expected = case preemptAt (oracle spec) writer 0 of
          Just p | p > d -> 1
          _              -> 2
    in counterexample (show (writer, preemptAt (oracle spec) writer 0, d))
     $ cover 10 (expected == 1) "window open"
     $ seen === expected

runSimOrThrowWith :: Preemption -> (forall s. IOSim s a) -> a
runSimOrThrowWith p a = either (error . show) id (runSimWith p a)

-- | Threads doing increments with delays between them.
incrementers :: [[Int]] -> IOSim s Int
incrementers delays = do
    v <- newTVarIO 0
    dones <- forM delays $ \ds -> do
      done <- newEmptyTMVarIO
      _ <- forkIO $ do
        forM_ ds $ \d -> do
          threadDelay (micros d)
          atomically (readTVar v >>= writeTVar v . succ)
          yield
        atomically (putTMVar done ())
      return done
    mapM_ (atomically . takeTMVar) dones
    readTVarIO v

genDelays :: Gen [[Int]]
genDelays = listOf1 (listOf1 (choose (0, 5_000)))

-- | Replaying the preemptions a random run made gives the same trace.
prop_replay :: Spec -> Property
prop_replay spec =
  forAllShrink genDelays shrink $ \delays ->
    let t  = runSimTraceWith (oracle spec) (incrementers delays)
        ps = preemptionsOf t
        t' = runSimTraceWith (preemptFromMap ps) (incrementers delays)
    in cover 50 (not (null ps)) "preempted"
     $ ppTrace_ t === ppTrace_ t'

-- | Every increment lands, whatever the preemptions.
prop_atomic :: Spec -> Property
prop_atomic spec =
  forAllShrink (choose (1, 6)) shrink $ \n ->
  forAllShrink (choose (1, 20)) shrink $ \k ->
    let r = runSimWith (oracle spec) $ do
          v <- newTVarIO (0 :: Int)
          dones <- forM [1 .. n] $ \_ -> do
            done <- newEmptyTMVarIO
            _ <- forkIO $ replicateM_ k (atomically (modifyTVar' v succ))
                       >> atomically (putTMVar done ())
            return done
          mapM_ (atomically . takeTMVar) dones
          readTVarIO v
    in counterexample (show r) (either (const False) (== n * k) r)

-- | A masked thread writes @a@ then @b@ in two transactions; it is killed at
-- @dK@.  A preemption between the transactions is no interruptible point, so
-- @a@ implies @b@.
prop_masked :: Spec -> NonNegative Int -> NonNegative Int -> Property
prop_masked spec (NonNegative dT) (NonNegative dK) =
    let (a, b, t) = runSimOrThrowWith (oracle spec) $ do
          va <- newTVarIO False
          vb <- newTVarIO False
          done <- newEmptyTMVarIO
          tid <- forkIO $ (threadDelay (micros dT) >> mask_ (do
                           atomically (writeTVar va True)
                           atomically (writeTVar vb True)))
                        `finally` atomically (putTMVar done ())
          threadDelay (micros dK + 0.000_000_5)
          killThread tid
          atomically (takeTMVar done)
          (,,) <$> readTVarIO va <*> readTVarIO vb <*> pure tid
        -- the thread's first yield point is after writing @a@
        kill   = micros dK + 0.000_000_5
        inside = case preemptAt (oracle spec) t 0 of
          Just p  -> micros dT < kill && kill < micros dT + p
          Nothing -> False
    in cover 5 inside "killed while preempted between the writes"
     $ counterexample (show (a, b))
     $ not a || b

data Interrupt = Interrupt
  deriving Show
instance Exception Interrupt

-- | A sleep (µs), on the scale of the oracle's delays.
newtype Sleep = Sleep Int
  deriving Show

instance Arbitrary Sleep where
  arbitrary = Sleep <$> choose (1, 20_000)
  shrink (Sleep n) = [ Sleep m | m <- shrink n, m >= 1 ]

-- | An unmasked thread is interrupted at @dK@ (perhaps while preempted after
-- its first commit), catches the exception and sleeps @dD@: it sleeps
-- exactly @dD@, even when its preemption would have ended during the sleep.
prop_interrupted :: Spec -> NonNegative Int -> Sleep -> Property
prop_interrupted spec (NonNegative dK) (Sleep dD) =
    let (slept, tid) = runSimOrThrowWith (oracle spec) $ do
          r <- newEmptyTMVarIO
          v <- newTVarIO (0 :: Int)
          t <- forkIO $ (atomically (writeTVar v 1) >> threadDelay 1000)
                 `catch` \Interrupt -> do
                   t0 <- getMonotonicTime
                   threadDelay (micros dD)
                   t1 <- getMonotonicTime
                   atomically (putTMVar r (t1 `diffTime` t0))
          threadDelay (micros dK + 0.000_000_5)
          throwTo t Interrupt
          (,) <$> atomically (takeTMVar r) <*> pure t
        kill   = micros dK + 0.000_000_5
        pre    = preemptAt (oracle spec) tid 0
        inside = maybe False (kill <) pre
        ends   = maybe False (< kill + micros dD) pre
    in cover 10 inside "interrupted while preempted"
     $ cover 5 (inside && ends) "preemption would end during the sleep"
     $ slept === micros dD
