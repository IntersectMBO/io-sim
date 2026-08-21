{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Control.Monad.IOSimPOR (tests) where

import Data.Fixed (Micro)
#if __GLASGOW_HASKELL__ >= 910
import Data.Foldable (traverse_)
#else
import Data.Foldable (foldl', traverse_)
#endif
import Data.Functor (($>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map

import System.Exit
import System.IO.Error (ioeGetErrorString, isUserError)

import Control.Exception (ArithException (..), AsyncException)
import Control.Monad
import Control.Monad.Fix

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim

import GHC.Generics

import Test.Control.Monad.IOSim (ActionDuration, TimeoutDuration,
           WithSanityCheck (..), ignoreSanityCheck, isSanityCheckIgnored,
           singleTimeoutExperiment, withSanityCheck)
import Test.Control.Monad.STM
import Test.Control.Monad.Utils

import Data.List.Trace qualified as Trace
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import System.IO.Unsafe

import Data.IORef
import Data.Time (secondsToDiffTime)
import Data.Void (Void)
import GHC.Conc (pseq)

tests :: TestTree
tests =
  testGroup "IOSimPOR"
  [ testGroup "schedule exploration"
    [ testProperty "propSimulates"    propSimulates
    , testProperty "propExploration"  propExploration
    , testGroup "issue113"
      [ testProperty "wakeup"         unit_issue113_wakeup
      , testProperty "racy"           unit_issue113_racy
      , testProperty "nonDep"         unit_issue113_nonDep
      ]
    -- , testProperty "propPermutations" propPermutations
    ]
  , testGroup "IO simulator properties"
    [ testProperty "read/write graph (IOSim)" (withNumTests 1000 prop_stm_graph_sim)
    , testGroup "timeouts"
      [ testProperty "IOSim"                  (withNumTests 1000 prop_timers_ST)
      , testProperty "IOSim: no deadlock"     prop_timeout_no_deadlock_Sim
      , testProperty "timeout"                prop_timeout
      , testProperty "timeouts"               prop_timeouts
      , testProperty "stacked timeouts"       prop_stacked_timeouts
      , testProperty "{register,thread}Delay" unit_registerDelay_threadDelay
      ]
    , testProperty "infinite simulation"      prop_explore_endless_simulation
    , testProperty "threadId order (IOSim)"   (withNumTests 1000 prop_threadId_order_order_Sim)
    , testProperty "forkIO order (IOSim)"     (withNumTests 1000 prop_fork_order_ST)
    , testGroup "throw/catch unit tests"
      [ testProperty "0" unit_catch_0
      , testProperty "1" unit_catch_1
      , testProperty "2" unit_catch_2
      , testProperty "3" unit_catch_3
      , testProperty "4" unit_catch_4
      , testProperty "5" unit_catch_5
      , testProperty "6" unit_catch_6
      ]
    , testGroup "masking state"
      [ testProperty "set (IOSim)"
      $ forall_masking_states unit_set_masking_state_ST

      , testProperty "unmask (IOSim)"
      $ forall_masking_states $ \ms  ->
        forall_masking_states $ \ms' -> unit_unmask_ST ms ms'

      , testProperty "fork (IOSim)"
      $ forall_masking_states unit_fork_masking_state_ST

      , testProperty "fork unmask (IOSim)"
      $ forall_masking_states $ \ms  ->
        forall_masking_states $ \ms' -> unit_fork_unmask_ST ms ms'

      , testProperty "catch (IOSim)"
      $ forall_masking_states unit_catch_throwIO_masking_state_ST

      , testProperty "catch: throwTo (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_ST

      , testProperty "catch: throwTo async (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_async_ST

      , testProperty "catch: throwTo async blocking (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_ST
      ]
    , testGroup "PropertyM callbacks"
      [ testProperty "happy path"      unit_monadicIOSimPOR_0
      , testProperty "failing assert"  unit_monadicIOSimPOR_1
      , testProperty "exception"       unit_monadicIOSimPOR_2
      ]
    , testProperty "evaluate unit test" unit_evaluate_0
    , testGroup "forkIO unit tests"
      [ testProperty "1" unit_fork_1
      ]
    , testGroup "async exception unit tests"
      [ testProperty "1"  unit_async_1
      , testProperty "3"  unit_async_3
      , testProperty "4"  unit_async_4
      , testProperty "5"  unit_async_5
      , testProperty "6"  unit_async_6
      , testProperty "7"  unit_async_7
      , testProperty "8"  unit_async_8
      , testProperty "9"  unit_async_9
      ]
    , testGroup "STM reference semantics"
      [ testProperty "Reference vs Sim"   prop_stm_referenceSim
      ]
    , testGroup "MonadFix instance"
      [ testProperty "purity"     prop_mfix_purity
      , testProperty "purity2"    prop_mfix_purity_2
      , testProperty "tightening" prop_mfix_left_shrinking
      , testProperty "lazy"       prop_mfix_lazy
      ]
    ]
  ]

data Step =
    WhenSet Int Int
  | ThrowTo Int
  | Delay Int
  | Timeout TimeoutStep
  deriving (Eq, Ord, Show)

data TimeoutStep =
    NewTimeout    Int
  | CancelTimeout
  | AwaitTimeout
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Step where
  arbitrary = frequency [(5,do m <- choose (1,20)
                               n <- choose (0,m)
                               return $ WhenSet m n),
                         (1,do NonNegative i <- arbitrary
                               return $ ThrowTo i),
                         (1,do Positive i <- arbitrary
                               return $ Delay i),
                         (1,Timeout <$> arbitrary)]

  shrink (WhenSet m n) = map (WhenSet m) (shrink n) ++
                         map (`WhenSet` n) (filter (>=n) (shrink m))
  shrink (ThrowTo i) = map ThrowTo (shrink i)
  shrink (Delay i)   = map Delay (shrink i)
  shrink (Timeout t) = map Timeout (shrink t)

instance Arbitrary TimeoutStep where
  arbitrary = do Positive i <- arbitrary
                 frequency $ map (fmap return) $
                   [(3,NewTimeout i),
                    (1,CancelTimeout),
                    (3,AwaitTimeout)]

  shrink = genericShrink


newtype Task = Task [Step]
  deriving (Eq, Ord, Show)

instance Arbitrary Task where
  arbitrary = do
    steps <- arbitrary
    return . Task $ normalize steps
  shrink (Task steps) =
    (Task <$> compressSteps steps) ++
    (Task . normalize <$> shrink steps)

normalize :: [Step] -> [Step]
normalize steps = plug steps wsSteps 1000000
  where wsSteps = reverse $ List.sort [s | s@(WhenSet _ _) <- steps]
        plug []              []               _ = []
        plug (WhenSet _ _:s) (WhenSet a b:ws) m = WhenSet (min a m) (min b m):plug s ws (min b m)
        plug (step:s)        ws               m = step:plug s ws m
        plug _               _                _ = error "plug: impossible"

compressSteps :: [Step] -> [[Step]]
compressSteps (WhenSet a b:WhenSet c d:steps) =
  [WhenSet a d:steps] ++ ((WhenSet a b:) <$> compressSteps (WhenSet c d:steps))
compressSteps (s:steps) = (s:) <$> compressSteps steps
compressSteps [] = []

newtype Tasks = Tasks [Task]
  deriving Show

instance Arbitrary Tasks where
  arbitrary = Tasks . fixSymbolicThreadIds <$> scale (min 20) arbitrary
  shrink (Tasks ts) = Tasks . fixSymbolicThreadIds <$>
         removeTask ts ++
         shrink ts ++
         shrinkDelays ts ++
         advanceThrowTo ts ++
         sortTasks ts

fixSymbolicThreadIds :: [Task] -> [Task]
fixSymbolicThreadIds tasks = mapSymThreadIds (`mod` length tasks) tasks

shrinkDelays :: [Task] -> [[Task]]
shrinkDelays tasks
  | null times = []
  | otherwise  = [map (Task . removeTime d) [steps | Task steps <- tasks]
                 | d <- times]
  where times = foldr List.union [] [scanl1 (+) [d | Delay d <- t] | Task t <- tasks]
        removeTime 0 steps = steps
        removeTime _ []    = []
        removeTime d (Delay d':steps)
          | d==d' = steps
          | d< d' = Delay (d'-d):steps
          | d> d' = removeTime (d-d') steps
        removeTime d (s:steps) =
          s:removeTime d steps

removeTask :: [Task] -> [[Task]]
removeTask tasks =
  [ mapThrowTos (fixup i) . map (dontThrowTo i) $ take i tasks++drop (i+1) tasks
  | i <- [0..length tasks-1]]
  where fixup i j | j>i       = j-1
                  | otherwise = j
        dontThrowTo i (Task steps) = Task (filter (/=ThrowTo i) steps)

advanceThrowTo :: [Task] -> [[Task]]
advanceThrowTo [] = []
advanceThrowTo (Task steps:ts) =
  ((:ts) . Task <$> advance steps) ++
  ((Task steps:) <$> advanceThrowTo ts)
  where advance (WhenSet a b:ThrowTo i:steppes) =
          [ThrowTo i:WhenSet a b:steppes] ++ (([WhenSet a b,ThrowTo i]++) <$> advance steppes)
        advance (s:steppes) = (s:) <$> advance steppes
        advance []          = []

mapSymThreadIds :: (Int -> Int) -> [Task] -> [Task]
mapSymThreadIds f tasks = map mapTask tasks
  where mapTask (Task steps) = Task (map mapStep steps)
        mapStep (ThrowTo i) = ThrowTo (f i)
        mapStep s           = s

mapThrowTos :: (Int -> Int) -> [Task] -> [Task]
mapThrowTos f tasks = map mapTask tasks
  where mapTask (Task steps) = Task (map mapStep steps)
        mapStep (ThrowTo i) = ThrowTo (f i)
        mapStep s           = s

sortTasks :: Ord a => [a] -> [[a]]
sortTasks (x:y:xs) | x>y = [y:x:xs] ++ ((x:) <$> sortTasks (y:xs))
sortTasks (x:xs)         = (x:) <$> sortTasks xs
sortTasks []             = []

data Compare = AreEqual | AreNotEqual
  deriving Show

instance Arbitrary Compare where
  arbitrary = frequency [(8, pure AreEqual),
                         (2, pure AreNotEqual)]

  shrink AreEqual    = []
  shrink AreNotEqual = [AreEqual]


interpret :: forall s.
             Compare
          -> TVar (IOSim s) Int
          -> TVar (IOSim s) [ThreadId (IOSim s)]
          -> (Int, Task)
          -> IOSim s (Async (IOSim s) ())
interpret cmp r t (tlbl, Task steps) = async $ do
    labelThisThread (show tlbl)
    (ts, timer) <- atomically $ do
      ts <- readTVar t
      check (not (null ts))
      timer <- newTVar Nothing
      return (ts,timer)
    mapM_ (interpretStep ts timer) steps
  where
    compareFn = case cmp of
      AreEqual    -> (==)
      AreNotEqual -> (/=)
    interpretStep :: [ThreadId (IOSim s)]
                  -> TVar (IOSim s) (Maybe (Timeout s))
                  -> Step
                  -> IOSim s ()
    interpretStep _ _ (WhenSet m n) = atomically $ do
      readTVar r >>= check . compareFn m
      writeTVar r n
    interpretStep ts _     (ThrowTo i) = throwTo (ts !! i) (ExitFailure 0)
    interpretStep _  _     (Delay i)   = threadDelay (fromIntegral i)
    interpretStep _  timer (Timeout tstep) = do
      timerVal <- readTVarIO timer
      case (timerVal,tstep) of
        (_,NewTimeout n)          -> do tout <- newTimeout (fromIntegral n)
                                        atomically $ writeTVar timer (Just tout)
        (Just tout,CancelTimeout) -> cancelTimeout tout
        (Just tout,AwaitTimeout)  -> atomically $ awaitTimeout tout >> return ()
        (Nothing,_)               -> return ()

runTasks :: Compare -> [Task] -> IOSim s (Int,Int)
runTasks cmp tasks = do
  let m = maximum [maxTaskValue t | Task t <- tasks]
  r  <- newTVarIO m
  traceTVarIO r (\_ a -> return (TraceString (show a)))
  t  <- newTVarIO []
  exploreRaces
  ts <- mapM (interpret cmp r t) (zip [1..] tasks)
  atomically $ writeTVar t (asyncThreadId <$> ts)
  traverse_ wait ts -- allow the SUT threads to run
  a  <- readTVarIO r
  return (m,a)

maxTaskValue :: [Step] -> Int
maxTaskValue (WhenSet m _:_) = m
maxTaskValue (_:t)           = maxTaskValue t
maxTaskValue []              = 0

propSimulates :: Compare -> Shrink2 Tasks -> Property
propSimulates cmp (Shrink2 (Tasks tasks)) =
  any (not . null . (\(Task steps)->steps)) tasks ==>
    let trace = runSimTrace (runTasks cmp tasks) in
    case traceResult False trace of
      Right (m,a) -> property (m >= a)
      Left (FailureInternal msg)
                  -> counterexample msg False
      Left x      -> counterexample (ppTrace trace)
                   $ counterexample (show x) True

-- NOTE: This property needs to be executed sequentially, otherwise it fails
-- undeterministically, which `exploreSimTraceST` does.
--
propExploration :: Compare -> Shrink2 Tasks -> Property
propExploration cmp (Shrink2 ts@(Tasks tasks)) =
  not (all (null . (\(Task steps)->steps)) tasks) ==>
    traceNoDuplicates $ \addTrace->
    exploreSimTrace (\a -> a { explorationDebugLevel = 0 })
                    (say (show ts) >> runTasks cmp tasks)
                    $ \_ trace ->
    addTrace trace
    -- TODO: for now @coot is leaving `Trace.ppTrace`, but once we change all
    -- assertions into `FailureInternal`, we can use `ppTrace` instead
    counterexample (Trace.ppTrace show (ppSimEvent 0 0 0) trace) $
             case traceResult False trace of
               Right (m,a) -> property (m >= a)
               Left (FailureInternal msg)
                           -> counterexample msg False
               Left _      -> property True


-- | This is a counterexample for a fix included in the commit: "io-sim-por:
-- fixed counterexample in issue #113".  There was a missing wakeup effect when
-- a thread terminates, for threads that were blocked on `throwTo`.
--
unit_issue113_wakeup :: Property
unit_issue113_wakeup =
    propExploration AreEqual (Shrink2 tasks)
  where
    tasks = Tasks [Task [ThrowTo 1, WhenSet 0 0],
                   Task [Delay 1],
                   Task [WhenSet 0 0],
                   Task [WhenSet 1 0, ThrowTo 1]]


-- | This test checks that we don't build a schedule which execute a racing
-- step that depends on something that wasn't added to `stepInfoNonDep`.
--
unit_issue113_racy :: Property
unit_issue113_racy =
    propExploration AreNotEqual (Shrink2 tasks)
  where
    tasks = Tasks [Task [WhenSet 1 0,ThrowTo 1],
                   Task [],
                   Task [ThrowTo 1,WhenSet 0 0],
                   Task [ThrowTo 1]]


-- | This test checks that we don't build a schedule which execute a non
-- dependent step that depends on something that wasn't added to
-- `stepInfoNonDep`.
--
unit_issue113_nonDep :: Property
unit_issue113_nonDep =
    propExploration AreNotEqual (Shrink2 tasks)
  where
    tasks = Tasks [Task [WhenSet 0 0],
                   Task [],
                   Task [WhenSet 0 0],
                   Task [ThrowTo 1,WhenSet 1 1],
                   Task [ThrowTo 5],
                   Task [ThrowTo 1]]


-- Testing propPermutations n should collect every permutation of [1..n] once only.
-- Test manually, and supply a small value of n.
propPermutations :: Int -> Property
propPermutations n =
  traceNoDuplicates $ \addTrace ->
  exploreSimTrace (withScheduleBound 10000) (doit n) $ \_ trace ->
    addTrace trace $
    let Right result = traceResult False trace
     in tabulate "Result" [show result] True

doit :: Int -> IOSim s [Int]
doit n = do
  r <- newTVarIO []
  exploreRaces
  mapM_ (\i -> forkIO $ atomically $ modifyTVar r (++[i])) [1..n]
  threadDelay 1
  readTVarIO r

traceNoDuplicates :: forall a b.
                     (Show a)
                  => ((a -> b -> b) -> Property)
                  -> Property
-- this NOINLINE pragma is useful for debugging if `r` didn't flow outside of
-- `traceNoDuplicate`.
{-# NOINLINE traceNoDuplicates #-}
traceNoDuplicates k = unsafePerformIO $ do
    r <- newIORef (Map.empty :: Map String Int)
    return $ r `pseq`
             (k (addTrace r) .&&. counterexample "trace counts" (maximum (Map.elems (traceCounts r)) === 1))
  where
    addTrace :: IORef (Map String Int) -> a -> b -> b
    addTrace r t x = unsafePerformIO $ do
      let s = show t
      atomicModifyIORef r
        (\m->
          let m' = Map.insertWith (+) s 1 m
          in (m', ())
        )
      return x

    traceCounts :: IORef (Map String Int) -> Map String Int
    traceCounts r = unsafePerformIO $ readIORef r


-- | Checks that IOSimPOR is capable of analysing an infinite simulation
-- lazily.
--
-- If the test fails with "outer timeout" error it means that 'exploreSimTrace'
-- is not lazy and there's something wrong with IOSimPOR implementation and
-- the trace is being forced way too soon. If, on the other hand, this property
-- fails with "inner timeout" error, it means that 'exploreSimTrace' is somewhat
-- lazy, however the trace is not being lazily generated.
--
--
prop_explore_endless_simulation :: Positive (Small Integer) -> Property
prop_explore_endless_simulation (Positive (Small finalTime)) =
    counterexample "outer timeout"
  $ label (show (secondsToDiffTime finalTime))
  $ within 15000000 -- 15 seconds
  $ exploreSimTrace
      id sim $ \_ trace -> do
        let l = takeWhile (\(t, _, _, _) -> t < Time (secondsToDiffTime finalTime))
              . traceEvents
              $ trace
         in ioProperty $ do
           r <- timeout 10 $ evaluate (foldl' (flip seq) True l)
           case r of
             Nothing -> return $ counterexample "inner timeout" False
             Just _  -> return (property True)
  where
    thread :: forall s. IOSim s Void
    thread = do
      threadDelay 1
      thread

    sim :: forall s. IOSim s Void
    sim = do
      exploreRaces
      withAsync thread wait

--
-- IOSim reused properties
--

--
-- Read/Write graph
--

prop_stm_graph_sim :: TestThreadGraph -> Property
prop_stm_graph_sim g =
  traceNoDuplicates $ \addTrace ->
    exploreSimTrace id (prop_stm_graph g) $ \_ trace -> do
      addTrace trace
      $ counterexample (Trace.ppTrace show show trace) $
        case traceResult False trace of
          Right () -> property True
          Left e   -> counterexample (show e) False
        -- TODO: Note that we do not use runSimStrictShutdown here to check
        -- that all other threads finished, but perhaps we should and structure
        -- the graph tests so that's the case.

prop_timers_ST :: TestMicro -> Property
prop_timers_ST (TestMicro xs) =
  let ds = map (realToFrac :: Micro -> DiffTime) xs
   in exploreSimTrace id (test_timers ds) $ \_ trace ->
        case traceResult False trace of
          Right a -> a
          Left e  -> counterexample (show e) False

--
-- Forking
--

prop_fork_order_ST :: Positive Int -> Property
prop_fork_order_ST n =
   exploreSimTrace id (test_fork_order n) $ \_ trace ->
     case traceResult False trace of
       Right a -> a
       Left e  -> counterexample (show e) False

prop_threadId_order_order_Sim :: Positive Int -> Property
prop_threadId_order_order_Sim n =
   exploreSimTrace id (test_threadId_order n) $ \_ trace ->
     case traceResult False trace of
       Right a -> a
       Left e  -> counterexample (show e) False

--
-- MonadFix properties
--

-- | Purity demands that @mfix (return . f) = return (fix f)@.
--
prop_mfix_purity :: Positive Int -> Property
prop_mfix_purity (Positive n) =
   exploreSimTrace id (mfix (return . factorial)) $ \_ trace ->
     case traceResult False trace of
       Right f -> f n === fix factorial n
       Left e  -> counterexample (show e) False
  where
    factorial :: (Int -> Int) -> Int -> Int
    factorial rec_ k = if k <= 1 then 1 else k * rec_ (k - 1)

prop_mfix_purity_2 :: [Positive Int] -> Property
prop_mfix_purity_2 as =
   -- note: both 'IOSim' expressions are equivalent using 'Monad' and
   -- 'Applicative' laws only.
   exploreSimTrace id (join $ mfix (return . recDelay)
                             <*> return as') (\_ trace ->
     case traceResult False trace of
       Right a -> a === expected
       Left e  -> counterexample (show e) False)
   .&&.
   exploreSimTrace id (mfix (return . recDelay) >>= ($ as')) (\_ trace ->
     case traceResult False trace of
       Right a -> a === expected
       Left e  -> counterexample (show e) False)
  where
    as' :: [Int]
    as' = getPositive `map` as

    -- recursive sum using 'threadDelay'
    recDelay :: MonadDelay m
             => ([Int] -> m Time)
             ->  [Int] -> m Time
    recDelay = \rec_ bs ->
                 case bs of
                  []        -> getMonotonicTime
                  (b : bs') -> threadDelay (realToFrac b)
                            >> rec_ bs'

    expected :: Time
    expected = foldl' (flip addTime)
                      (Time 0)
                      (realToFrac `map` as')

prop_mfix_left_shrinking
    :: Int
    -> NonNegative Int
    -> Positive Int
    -> Property
prop_mfix_left_shrinking n (NonNegative d) (Positive i) =
   let mn :: IOSim s Int
       mn = do say ""
               threadDelay (realToFrac d)
               return n
    in exploreSimTrace id (mfix (\rec_ -> mn >>= \a ->
                                    threadDelay (realToFrac d) $> a : rec_))
                          (\_ trace1 ->
       exploreSimTrace id (mn >>= \a ->
                              mfix (\rec_ ->
                                       threadDelay (realToFrac d) $> a : rec_))
                          (\_ trace2 ->
         case (traceResult False trace1, traceResult False trace2) of
           (Right a , Right b)  -> take i a === take i b
           (Left  e , Right _)  -> counterexample (show e) False
           (Right _ , Left  e)  -> counterexample (show e) False
           (Left  e , Left  e') -> counterexample (show e ++ " " ++ show e') False))


-- | 'Example 8.2.1' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_lazy :: NonEmptyList Char
               -> Property
prop_mfix_lazy (NonEmpty env) =
   exploreSimTrace id (withEnv (mfix . replicateHeadM)) (\_ trace ->
     case traceResult False trace of
       Right a -> take samples a === replicate samples (head env)
       Left e  -> counterexample (show e) False)
    where
      samples :: Int
      samples = 10

      replicateHeadM :: MonadFail m
                     => m Char
                     -> String -> m String
      replicateHeadM getChar_ as = do
        -- Note: 'getChar' will be executed only once! This follows from 'fixIO`
        -- semantics.
        a <- getChar_
        return (a : as)

      -- construct 'getChar' using the simulated environment
      withEnv :: (

                   MonadFail m,

                   MonadSTM  m
                 )
              => (m Char -> m a) -> m a
      withEnv k = do
        v <- newTVarIO env
        let getChar_ =
              atomically $ do
                as <- readTVar v
                case as of
                  [] -> error "withEnv: runtime error"
                  (a : as') -> writeTVar v as'
                            $> a
        k getChar_

--
-- Syncronous exceptions
--

unit_catch_0, unit_catch_1, unit_catch_2, unit_catch_3, unit_catch_4,
  unit_catch_5, unit_catch_6, unit_fork_1
  :: Property


--   exploreSimTrace id (withEnv (mfix . replicateHeadM)) (\_ trace ->
--     case traceResult False trace of
--       Right a -> take samples a === replicate samples (head env)
--       Left e  -> counterexample (show e) False)

-- unhandled top level exception
unit_catch_0 =
  exploreSimTrace id example $ \_ trace ->
    counterexample (List.intercalate "\n" $ map show $ traceEvents trace) $
    counterexample (show $ selectTraceSay trace) $
    selectTraceSay trace === ["before"]
    .&&.
    case traceResult True trace of
      Left (FailureException e) -> property ((Just DivideByZero ==) $ fromException e)
      _                         -> property False
 where
  example :: IOSim s ()
  example = do
    say "before"
    _ <- throwIO DivideByZero
    say "after"

-- normal execution of a catch frame
unit_catch_1 =
  exploreSimTrace id (do catch (say "inner")
                               (\(_e :: IOError) -> say "handler")
                         say "after")
                         $ \_ trace ->
    selectTraceSay trace === ["inner", "after"]

-- catching an exception thrown in a catch frame
unit_catch_2 =
  exploreSimTrace id
                  (do catch (do say "inner1"
                                _ <- throwIO DivideByZero
                                say "inner2")
                            (\(_e :: ArithException) -> say "handler")
                      say "after"
                   ) $ \_ trace ->
    selectTraceSay trace === ["inner1", "handler", "after"]

-- not catching an exception of the wrong type
unit_catch_3 =
  exploreSimTrace id
                  (do catch (do say "inner"
                                throwIO DivideByZero)
                            (\(_e :: IOError) -> say "handler")
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["inner"]


-- catching an exception in an outer handler
unit_catch_4 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(_e :: IOError) -> say "handler1"))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["inner", "handler2", "after"]


-- catching an exception in the inner handler
unit_catch_5 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(_e :: ArithException) -> say "handler1"))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                  ) $ \_ trace ->
     selectTraceSay trace === ["inner", "handler1", "after"]

-- catching an exception in the inner handler, rethrowing and catching in outer
unit_catch_6 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(e :: ArithException) -> do
                                     say "handler1"
                                     throwIO e))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                   ) $ \_ trace ->
    selectTraceSay trace === ["inner", "handler1", "handler2", "after"]

unit_monadicIOSimPOR_0, unit_monadicIOSimPOR_1, unit_monadicIOSimPOR_2
  :: Property

-- | Basic success case: a PropertyM test can be executed through IOSimPOR.
unit_monadicIOSimPOR_0 =
  monadicIOSimPOR_ $ do
    x <- run (pure (1 :: Int))
    assert (x == 1)

-- | Failing assertions inside PropertyM are reported as property failures.
unit_monadicIOSimPOR_1 =
  expectFailure $
    monadicIOSimPOR_ $ do
      x <- run (pure (1 :: Int))
      assert (x == 2)

-- | Exceptions thrown by the simulation are turned into failing properties via
-- `traceResult False`, matching the manual workaround from issue #229.
unit_monadicIOSimPOR_2 =
  expectFailure $
    monadicIOSimPOR_ $ do
      _ <- run $ evaluate (error "boom" :: ())
      assert True

-- evaluate should catch pure errors
unit_evaluate_0 :: Property
unit_evaluate_0 =
    -- This property also fails if the @error@ is not caught by the sim monad
    -- and instead reaches the QuickCheck driver.
    -- property $ isLeft $ runSim $ evaluate (error "boom" :: ())
  exploreSimTrace id (evaluate (error "boom" :: ())) $ \_ trace ->
    case traceResult False trace of
      Right _ -> counterexample "didn't fail" False
      Left _  -> property True


-- Try works and we can pass exceptions back from threads.
-- And terminating with an exception is reported properly.
unit_fork_1 =
  exploreSimTrace id example $ \_ trace ->
    selectTraceSay trace === ["parent", "user error (oh noes!)"]
    .&&. case traceResult True trace of
          Left (FailureException e)
            | Just ioe <- fromException e
            , isUserError ioe
            , ioeGetErrorString ioe == "oh noes!" -> property True
          _                                       -> property False
  where
    example :: IOSim s ()
    example = do
      resVar <- newEmptyTMVarIO
      void $ forkIO $ do
        res <- try (fail "oh noes!")
        atomically (putTMVar resVar (res :: Either SomeException ()))
      say "parent"
      Left e <- atomically (takeTMVar resVar)
      say (show e)
      throwIO e


--
-- Asyncronous exceptions
--

unit_async_1, unit_async_2, unit_async_3, unit_async_4,
  unit_async_5, unit_async_6, unit_async_7, unit_async_8,
  unit_async_9
  :: Property


unit_async_1 =
  exploreSimTrace id
                  (do tid <- myThreadId
                      say "before"
                      throwTo tid DivideByZero
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["before"]


unit_async_2 =
    runSimTraceSay
      (do tid <- myThreadId
          catch (do say "before"
                    throwTo tid DivideByZero
                    say "never")
                (\(_e :: ArithException) -> say "handler"))
 ===
   ["before", "handler"]


unit_async_3 =
  exploreSimTrace id
                  (do tid <- forkIO $ say "child"
                      threadDelay 1
                      -- child has already terminated when we throw the async exception
                      throwTo tid DivideByZero
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "parent done"]


unit_async_4 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                              say "child"
                              catch (atomically retry)
                                    (\(_e :: ArithException) -> say "handler")
                              say "child done"
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "handler", "child done", "parent done"]


unit_async_5 =
  exploreSimTrace id
                  (do tid <- forkIO $ mask_ $
                               do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked, do a blocking (interruptible) operation
                                 catch (atomically retry)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "handler", "child done", "parent done"]


unit_async_6 =
  exploreSimTrace id
                  (do tid <- forkIO $
                               mask_ $ do
                                 say "child"
                                 threadDelay 1
                                 fail "oh noes!"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      -- throwTo blocks but then unblocks because the child dies
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "parent done"]


unit_async_7 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                               uninterruptibleMask_ $ do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked, do a blocking (interruptible) operation
                                 catch (threadDelay 1)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                               say "never"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "child done", "parent done"]


unit_async_8 =
  exploreSimTrace id
                  (uninterruptibleMask_ $ do
                     tid <- forkIO $ atomically retry
                     throwTo tid DivideByZero) $ \_ trace ->
    case traceResult False trace of
      Left FailureDeadlock {} -> property True
      _                       -> property False


unit_async_9 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                               uninterruptibleMask_ $ do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked do a blocking operation, but this is
                                 -- an uninterruptible mask so nothing happens
                                 catch (threadDelay 1)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                               say "never"
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "child done", "parent done"]


--
-- Tests vs STM operational semantics
--

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the IO simulator's STM implementation.
--
prop_stm_referenceSim :: SomeTerm -> Property
prop_stm_referenceSim t =
  exploreSimTrace id (prop_stm_referenceM t) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

prop_timeout_no_deadlock_Sim :: Property
prop_timeout_no_deadlock_Sim = -- runSimOrThrow prop_timeout_no_deadlockM
  exploreSimTrace id prop_timeout_no_deadlockM $ \_ trace ->
    case traceResult False trace of
      Right a -> property a
      Left e  -> counterexample (show e) False

prop_timeout
    :: TimeoutDuration
    -> ActionDuration
    -> Property
prop_timeout intendedTimeoutDuration intendedActionDuration =
    exploreSimTrace id experiment $ \_ trace ->
        case traceResult False trace of
          Right a -> a
          Left e  -> counterexample (show e) False
  where
    experiment :: IOSim s Property
    experiment = do
      exploreRaces
      withSanityCheck <$> singleTimeoutExperiment intendedTimeoutDuration intendedActionDuration

prop_timeouts
    :: [(TimeoutDuration, ActionDuration)]
    -> Property
prop_timeouts times = exploreSimTrace id experiment $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False
  where
    experiment :: IOSim s Property
    experiment = do
      exploreRaces
      conjoin' <$>
        sequence
          [ fmap (counterexample ("failure on timeout test #" ++ show n))
            <$> singleTimeoutExperiment intendedTimeoutDuration
                                        intendedActionDuration
          | ((intendedTimeoutDuration,
              intendedActionDuration), n) <- zip times [1 :: Int ..] ]

    maxFailures = 0

    conjoin' :: [WithSanityCheck Property] -> Property
    conjoin' props =
           conjoin (ignoreSanityCheck `map` props)
      .&&. let numFailures = length (filter isSanityCheckIgnored props)
           in counterexample
               ("too many failures: " ++ show numFailures ++ " ≰ " ++ show maxFailures)
               (numFailures <= maxFailures)


prop_stacked_timeouts :: DiffTime
                      -> DiffTime
                      -> DiffTime
                      -> Property
prop_stacked_timeouts timeout0 timeout1 actionDuration =
    exploreSimTrace id experiment $ \_ trace ->
      case traceResult False trace of
        Right result -> result === predicted
        Left e       -> counterexample (show e) False
  where
    experiment :: IOSim s (Maybe (Maybe ()))
    experiment = exploreRaces
              >> timeout timeout0 (timeout timeout1 (threadDelay actionDuration))

    predicted | timeout0 == 0
              = Nothing

              | timeout1 == 0
              = Just Nothing

              -- This differs from `IOSim` case; `IOSimPOR` is using
              -- different scheduler.
              | actionDuration < min timeout0 timeout1
              = Just (Just ())

              | timeout0 < timeout1
              = Nothing

              | otherwise -- i.e. timeout0 >= timeout1
              = Just Nothing

unit_registerDelay_threadDelay :: Property
unit_registerDelay_threadDelay =
    exploreSimTrace id experiment $ \_ trace ->
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    experiment :: IOSim s Property
    experiment = do
      v0 <- registerDelay 2
      v1 <- newTVarIO False

      _ <- forkIO $ do
        threadDelay 1
        atomically $ writeTVar v1 True

      atomically $ do
        b0 <- readTVar v0
        b1 <- readTVar v1
        check $ b0 || b1

      let delay = 2
      t0 <- getMonotonicTime
      threadDelay delay
      t1 <- getMonotonicTime

      return (t1 `diffTime` t0 === delay)

unit_timeouts_and_async_exceptions_1 :: Property
unit_timeouts_and_async_exceptions_1 =
    exploreSimTrace id experiment $ \_ trace ->
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      exploreRaces
      tid <- forkIO $ void $
        timeout delay (atomically retry)

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True


unit_timeouts_and_async_exceptions_2 :: Property
unit_timeouts_and_async_exceptions_2 =
    exploreSimTrace id experiment $ \_ trace ->
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      exploreRaces
      tid <- forkIO $ void $
        timeout delay (atomically retry) `catch` (\(_ :: AsyncException) -> return Nothing)

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True


unit_timeouts_and_async_exceptions_3 :: Property
unit_timeouts_and_async_exceptions_3 =
    exploreSimTrace id experiment $ \_ trace ->
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      exploreRaces
      tid <- forkIO $ void $
        timeout delay (atomically retry `catch` (\(_ :: AsyncException) -> return ()))

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True

--
-- MonadMask properties
--

unit_set_masking_state_ST :: MaskingState -> Property
unit_set_masking_state_ST ms =
  exploreSimTrace id (prop_set_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_unmask_ST :: MaskingState -> MaskingState -> Property
unit_unmask_ST ms ms' =
  exploreSimTrace id (prop_unmask ms ms') $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_fork_masking_state_ST :: MaskingState -> Property
unit_fork_masking_state_ST ms =
  exploreSimTrace id (prop_fork_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_fork_unmask_ST :: MaskingState -> MaskingState -> Property
unit_fork_unmask_ST ms ms' =
  exploreSimTrace id (prop_fork_unmask ms ms') $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwIO_masking_state_ST :: MaskingState -> Property
unit_catch_throwIO_masking_state_ST ms =
  exploreSimTrace id (prop_catch_throwIO_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_async_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state_async ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_async_mayblock_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state_async_mayblock ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False
