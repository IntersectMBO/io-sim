{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- `-fno-full-laziness` is needed for `discardAfter` to work correctly, see
-- `unit_discardAfter` below.
{-# OPTIONS_GHC -fno-full-laziness #-}

module Test.Control.Monad.IOSim
  ( tests
  , TestThreadGraph (..)
    -- * Timeout tests
  , WithSanityCheck (..)
  , withSanityCheck
  , ignoreSanityCheck
  , isSanityCheckIgnored
  , TimeoutConstraints
  , TimeoutDuration
  , ActionDuration
  , singleTimeoutExperiment
  ) where

import Data.Either (isLeft)
import Data.Fixed (Micro)
#if __GLASGOW_HASKELL__ < 910
import Data.Foldable (foldl')
#endif
import Data.Functor (($>))
import Data.Time.Clock (picosecondsToDiffTime)

import Control.Exception (ArithException (..), AsyncException)
import Control.Monad
import Control.Monad.Fix
import System.IO.Error (ioeGetErrorString, isUserError)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TVar qualified as LazySTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim

import Test.Control.Monad.STM
import Test.Control.Monad.Utils

import Test.QuickCheck
import Test.QuickCheck.Property as QC
import Test.Tasty hiding (after)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup "IOSim"
  [ testProperty "read/write graph (IO)"    prop_stm_graph_io
  , testProperty "read/write graph (IOSim)" (withNumTests 1000 prop_stm_graph_sim)
  , testGroup "timeouts"
    [ testProperty "IOSim"                  (withNumTests 1000 prop_timers_ST)
    -- fails since we just use `threadDelay` to schedule timers in `IO`.
    , testProperty "IO"                     (expectFailure prop_timers_IO)
    , testProperty "IOSim: no deadlock"     prop_timeout_no_deadlock_Sim
    , testProperty "IO: no deadlock"        prop_timeout_no_deadlock_IO
    , testProperty "timeout"                prop_timeout
    , testProperty "timeouts"               prop_timeouts
    , testProperty "stacked timeouts"       prop_stacked_timeouts
    , testProperty "async exceptions 1"     unit_timeouts_and_async_exceptions_1
    , testProperty "async exceptions 2"     unit_timeouts_and_async_exceptions_2
    , testProperty "async exceptions 3"     unit_timeouts_and_async_exceptions_3
    , testProperty "threadDelay and STM"    unit_threadDelay_and_stm
    , testProperty "{register,thread}Delay" unit_registerDelay_threadDelay
    , testProperty "throwTo and STM"        unit_throwTo_and_stm
    ]
  , testGroup "QuickCheck"
    [ testProperty "timeout: discardAfter"  unit_discardAfter
    , testProperty "timeout: within"        unit_within
    ]
  , testProperty "threadId order (IOSim)"   (withNumTests 1000 prop_threadId_order_order_Sim)
  , testProperty "forkIO order (IOSim)"     (withNumTests 1000 prop_fork_order_ST)
  , testProperty "order (IO)"               (expectFailure prop_fork_order_IO)
  , testProperty "STM wakeup order"         prop_wakeup_order_ST
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
    [ testProperty "set (IO)"
    $ forall_masking_states unit_set_masking_state_IO
    , testProperty "set (IOSim)"
    $ forall_masking_states unit_set_masking_state_ST

    , testProperty "unmask (IO)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_unmask_IO ms ms'
    , testProperty "unmask (IOSim)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_unmask_ST ms ms'

    , testProperty "fork (IO)"
    $ forall_masking_states unit_fork_masking_state_IO
    , testProperty "fork (IOSim)"
    $ forall_masking_states unit_fork_masking_state_ST

    , testProperty "fork unmask (IO)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_fork_unmask_IO ms ms'
    , testProperty "fork unmask (IOSim)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_fork_unmask_ST ms ms'

    , testProperty "catch (IO)"
    $ forall_masking_states unit_catch_throwIO_masking_state_IO
    , testProperty "catch (IOSim)"
    $ forall_masking_states unit_catch_throwIO_masking_state_ST

    , testProperty "catch: throwTo (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_IO
    , testProperty "catch: throwTo (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_ST

    , testProperty "catch: throwTo async (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_IO
    , testProperty "catch: throwTo async (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_ST

    , testProperty "catch: throwTo async blocking (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_IO
    , testProperty "catch: throwTo async blocking (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_ST
    ]
  , testProperty "evaluate unit test" unit_evaluate_0
  , testGroup "forkIO unit tests"
    [ testProperty "1" unit_fork_1
    , testProperty "2" unit_fork_2
    ]
  , testGroup "async exception unit tests"
    [ testProperty "1"  unit_async_1
    , testProperty "2"  unit_async_2
    , testProperty "3"  unit_async_3
    , testProperty "4"  unit_async_4
    , testProperty "5"  unit_async_5
    , testProperty "6"  unit_async_6
    , testProperty "7"  unit_async_7
    , testProperty "8"  unit_async_8
    , testProperty "9"  unit_async_9
    , testProperty "10" unit_async_10
    , testProperty "11" unit_async_11
    , testProperty "12" unit_async_12
    , testProperty "13" unit_async_13
    , testProperty "14" unit_async_14
    , testProperty "15" unit_async_15
    , testProperty "16" unit_async_16
    ]
  , testGroup "STM reference semantics"
    [ testProperty "Reference vs IO"    prop_stm_referenceIO
    , testProperty "Reference vs Sim"   prop_stm_referenceSim
    ]
  , testGroup "MonadFix instances"
    [ testGroup "IOSim"
      [ testProperty "purity"     prop_mfix_purity_IOSim
      , testProperty "purity2"    prop_mfix_purity_2
      , testProperty "tightening" prop_mfix_left_shrinking_IOSim
      , testProperty "lazy"       prop_mfix_lazy
      , testProperty "recdata"    prop_mfix_recdata
      ]
    , testGroup "STM"
      [ testProperty "purity"     prop_mfix_purity_STM
      , testProperty "tightening" prop_mfix_left_shrinking_STM
      ]
    ]
  -- NOTE: Most of the tests below only work because the io-sim
  -- scheduler works the way it does.
  , testGroup "MonadTimerCancellable"
    [ testProperty "registerDelayCancellable (IOSim impl)"
        prop_registerDelayCancellable_IOSim
    , testProperty "registerDelayCancellable (IO impl)"
        prop_registerDelayCancellable_IO
    ]
  , testGroup "MonadSTM"
    [ testGroup "flushTQueue"
      [ testProperty "empties the queue IO" prop_flushTQueueEmpties_IO
      , testProperty "empties the queue IOSim" prop_flushTQueueEmpties_IOSim
      , testProperty "maintains FIFO order IO" prop_flushTQueueOrder_IO
      , testProperty "maintains FIFO order IOSim" prop_flushTQueueOrder_IOSim
      ]
    , testGroup "flushTBQueue"
      [ testProperty "empties the queue IO" prop_flushTBQueueEmpties_IO
      , testProperty "empties the queue IOSim" prop_flushTBQueueEmpties_IOSim
      , testProperty "maintains FIFO order IO" prop_flushTBQueueOrder_IO
      , testProperty "maintains FIFO order IOSim" prop_flushTBQueueOrder_IOSim
      ]
    , testGroup "tryReadTBQueue"
      [ testProperty "works correctly when the queue is empty IO" prop_tryReadEmptyTBQueue_IO
      , testProperty "works correctly when the queue is empty IOSim" prop_tryReadEmptyTBQueue_IOSim
      ]
    ]
  ]

--
-- Read/Write graph
--

prop_stm_graph_io :: TestThreadGraph -> Property
prop_stm_graph_io g =
  ioProperty $
    prop_stm_graph g

prop_stm_graph_sim :: TestThreadGraph -> Bool
prop_stm_graph_sim g =
    case runSim (prop_stm_graph g) of
       Right () -> True
       _        -> False
    -- TODO: Note that we do not use runSimStrictShutdown here to check
    -- that all other threads finished, but perhaps we should and structure
    -- the graph tests so that's the case.

prop_timers_ST :: TestMicro -> Property
prop_timers_ST (TestMicro xs) =
  let ds = map (realToFrac :: Micro -> DiffTime) xs
  in runSimOrThrow $ test_timers ds

prop_timers_IO :: [Positive Int] -> Property
prop_timers_IO = ioProperty . test_timers
               . map (microsecondsToDiffTime . (*100) . getPositive)
  where
    microsecondsToDiffTime :: Int -> DiffTime
    microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000) . toInteger

--
-- Forking
--

prop_fork_order_ST :: Positive Int -> Property
prop_fork_order_ST n = runSimOrThrow $ test_fork_order n

prop_fork_order_IO :: Positive Int -> Property
prop_fork_order_IO = ioProperty . test_fork_order

prop_threadId_order_order_Sim :: Positive Int -> Property
prop_threadId_order_order_Sim n = runSimOrThrow $ test_threadId_order n

prop_wakeup_order_ST :: Property
prop_wakeup_order_ST = runSimOrThrow $ test_wakeup_order

--
-- MonadFix properties
--

-- | Purity demands that @mfix (return . f) = return (fix f)@.
--
prop_mfix_purity_m :: forall m. MonadFix m => Positive Int -> m Bool
prop_mfix_purity_m (Positive n) =
    (== fix factorial n) . ($ n) <$> mfix (return . factorial)
  where
    factorial :: (Int -> Int) -> Int -> Int
    factorial = \rec_ k -> if k <= 1 then 1 else k * rec_ (k - 1)

prop_mfix_purity_IOSim :: Positive Int -> Bool
prop_mfix_purity_IOSim a = runSimOrThrow $ prop_mfix_purity_m a

prop_mfix_purity_STM:: Positive Int -> Bool
prop_mfix_purity_STM a = runSimOrThrow $ atomically $ prop_mfix_purity_m a

prop_mfix_purity_2 :: [Positive Int] -> Bool
prop_mfix_purity_2 as =
    -- note: both 'IOSim' expressions are equivalent using 'Monad' and
    -- 'Applicative' laws only.
      runSimOrThrow (join $  mfix (return . recDelay)
                         <*> return as')
      == expected
    &&
      runSimOrThrow (mfix (return . recDelay) >>= ($ as'))
      == expected
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


prop_mfix_left_shrinking_IOSim
    :: Int
    -> NonNegative Int
    -> Positive Int
    -> Bool
prop_mfix_left_shrinking_IOSim n (NonNegative d) (Positive i) =
   let mn :: IOSim s Int
       mn = do say ""
               threadDelay (realToFrac d)
               return n
   in
        take i
        (runSimOrThrow $
          mfix (\rec_ -> mn >>= \a -> do
                  threadDelay (realToFrac d) $> a : rec_))
      ==
        take i
        (runSimOrThrow $
          mn >>= \a ->
            (mfix (\rec_ -> do
              threadDelay (realToFrac d) $> a : rec_)))


prop_mfix_left_shrinking_STM
    :: Int
    -> Positive Int
    -> Bool
prop_mfix_left_shrinking_STM n (Positive i) =
   let mn :: STMSim s Int
       mn = do say ""
               return n
   in
        take i
        (runSimOrThrow $ atomically $
          mfix (\rec_ -> mn >>= \a -> return $ a : rec_))
      ==
        take i
        (runSimOrThrow $ atomically $
          mn >>= \a ->
            (mfix (\rec_ -> return $ a : rec_)))



-- | 'Example 8.2.1' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_lazy :: NonEmptyList Char
               -> Bool
prop_mfix_lazy (NonEmpty env) =
         take samples
           (runSimOrThrow (withEnv (mfix . replicateHeadM)))
      == replicate samples (head env)
    where
      samples :: Int
      samples = 10

      replicateHeadM ::
                        (
#if MIN_VERSION_base(4,13,0)
                          MonadFail m
#else
                          Monad m
#endif
                        )
                     => m Char
                     -> [Char] -> m [Char]
      replicateHeadM getChar_ as = do
        -- Note: 'getChar' will be executed only once! This follows from 'fixIO`
        -- semantics.
        a <- getChar_
        return (a : as)

      -- construct 'getChar' using the simulated environment
      withEnv :: (
#if MIN_VERSION_base(4,13,0)
                   MonadFail m,
#endif
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


-- | 'Example 8.2.3' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_recdata :: Property
prop_mfix_recdata = ioProperty $ do
    expected <- experiment
    let res = runSimOrThrow experiment
    return $
      take samples res
      ==
      take samples expected
  where
    samples :: Int
    samples = 10

    experiment :: ( MonadSTM m
                  , MonadFix m
                  )
               => m [Int]
    experiment = do
      (_, y) <-
        mfix (\ ~(x, _) -> do
                y <- LazySTM.newTVarIO x
                return (1:x, y)
             )
      atomically (LazySTM.readTVar y)

--
-- Synchronous exceptions
--

unit_catch_0, unit_catch_1, unit_catch_2, unit_catch_3, unit_catch_4,
  unit_catch_5, unit_catch_6,
  unit_fork_1, unit_fork_2
  :: Property

-- unhandled top level exception
unit_catch_0 = once $
      runSimTraceSay example === ["before"]
 .&&. case traceResult True (runSimTrace example) of
        Left (FailureException e) -> property (maybe False (==DivideByZero) $ fromException e)
        _                         -> property False

 where
  example :: IOSim s ()
  example = do
    say "before"
    _ <- throwIO DivideByZero
    say "after"

-- normal execution of a catch frame
unit_catch_1 = once $
    runSimTraceSay
      (do catch (say "inner") (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner", "after"]


-- catching an exception thrown in a catch frame
unit_catch_2 = once $
    runSimTraceSay
      (do catch (do say "inner1"
                    _ <- throwIO DivideByZero
                    say "inner2")
                (\(_e :: ArithException) -> say "handler")
          say "after"
      )
 ===
    ["inner1", "handler", "after"]


-- not catching an exception of the wrong type
unit_catch_3 = once $
    runSimTraceSay
      (do catch (do say "inner"
                    throwIO DivideByZero)
                (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner"]


-- catching an exception in an outer handler
unit_catch_4 = once $
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: IOError) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler2", "after"]


-- catching an exception in the inner handler
unit_catch_5 = once $
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: ArithException) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "after"]


-- catching an exception in the inner handler, rethrowing and catching in outer
unit_catch_6 = once $
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(e :: ArithException) -> do
                           say "handler1"
                           throwIO e))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "handler2", "after"]


-- evaluate should catch pure errors
unit_evaluate_0 :: Property
unit_evaluate_0 = once $
    -- This property also fails if the @error@ is not caught by the sim monad
    -- and instead reaches the QuickCheck driver.
    property $ isLeft $ runSim $ evaluate (error "boom" :: ())


-- The sim terminates when the main thread terminates
unit_fork_1 = once $
      runSimTraceSay example === ["parent"]
 .&&. case traceResult True (runSimTrace example) of
        Left FailureSloppyShutdown{} -> property True
        _                            -> property False
  where
    example :: IOSim s ()
    example = do
      void $ forkIO $ say "child"
      say "parent"

-- Try works and we can pass exceptions back from threads.
-- And terminating with an exception is reported properly.
unit_fork_2 = once $
      runSimTraceSay example === ["parent", "user error (oh noes!)"]
 .&&. case traceResult True (runSimTrace example) of
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

unit_async_1, unit_async_2, unit_async_3, unit_async_4, unit_async_5,
  unit_async_6, unit_async_7, unit_async_8, unit_async_9, unit_async_10,
  unit_async_11, unit_async_12, unit_async_13, unit_async_14, unit_async_15,
  unit_async_16
  :: Property


unit_async_1 = once $
    runSimTraceSay
      (do mtid <- myThreadId
          say ("main " ++ show mtid)
          ctid <- forkIO $ do tid <- myThreadId
                              say ("child " ++ show tid)
          say ("parent " ++ show ctid)
          threadDelay 1
      )
 ===
   ["main ThreadId []", "parent ThreadId [1]", "child ThreadId [1]"]


unit_async_2 = once $
    runSimTraceSay
      (do tid <- myThreadId
          say "before"
          throwTo tid DivideByZero
          say "after"
      )
 ===
   ["before"]


unit_async_3 = once $
    runSimTraceSay
      (do tid <- myThreadId
          catch (do say "before"
                    throwTo tid DivideByZero
                    say "never")
                (\(_e :: ArithException) -> say "handler"))
 ===
   ["before", "handler"]


unit_async_4 = once $
    runSimTraceSay
      (do tid <- forkIO $ say "child"
          threadDelay 1
          -- child has already terminated when we throw the async exception
          throwTo tid DivideByZero
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_5 = once $
    runSimTraceSay
      (do tid <- forkIO $ do
                   say "child"
                   catch (atomically retry)
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "handler", "child done", "parent done"]


unit_async_6 = once $
    runSimTraceSay
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
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_7 = once $
    runSimTraceSay
      (do tid <- forkIO $
                   mask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_8 = once $
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do mask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_9 = once $
    runSimTraceSay
      (do tid <- forkIO $
                   mask_ $ do
                     say "child"
                     threadDelay 1
                     fail "oh noes!"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          -- throwTo blocks but then unblocks because the child dies
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_10 = once $
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $ do
                      threadDelay 1
                      say "child 2"
                      -- this one blocks, since child 1 is running with
                      -- async exceptions masked
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, child 2 does not have exceptions
          -- masked (and it is blocked in an interruptible throwTo)
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]


unit_async_11 = once $
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $
                    -- Same as unit_async_10 but we run masked here
                    -- this is subtle: when the main thread throws the
                    -- exception it raises the exception here even though
                    -- it is masked because this thread is blocked in the
                    -- throwTo and so is interruptible.
                    mask_ $ do
                      threadDelay 1
                      say "child 2"
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, even though child 2 has exceptions
          -- masked, since it is blocked in an interruptible throwTo
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]


unit_async_12 = once $
    runSimTraceSay
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
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_13 = once $
    case runSim
           (uninterruptibleMask_ $ do
              tid <- forkIO $ atomically retry
              throwTo tid DivideByZero)
       of Left FailureDeadlock {} -> property True
          _                       -> property False


unit_async_14 = once $
    runSimTraceSay
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
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_15 = once $
    runSimTraceSay
      (do tid <- forkIO $
                   uninterruptibleMask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_16 = once $
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do uninterruptibleMask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


--
-- Tests vs STM operational semantics
--

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the real IO STM implementation.
--
prop_stm_referenceIO :: SomeTerm -> Property
prop_stm_referenceIO t =
    ioProperty (prop_stm_referenceM t)

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the IO simulator's STM implementation.
--
prop_stm_referenceSim :: SomeTerm -> Property
prop_stm_referenceSim t =
    runSimOrThrow (prop_stm_referenceM t)

--
-- MonadTimer
--

prop_timeout_no_deadlock_Sim :: Bool
prop_timeout_no_deadlock_Sim = runSimOrThrow prop_timeout_no_deadlockM

prop_timeout_no_deadlock_IO :: Property
prop_timeout_no_deadlock_IO = ioProperty prop_timeout_no_deadlockM

type TimeoutDuration = DiffTime
type ActionDuration  = DiffTime
type TimeoutConstraints m =
      ( MonadAsync m
      , MonadDelay m
      , MonadFork  m
      , MonadTime  m
      , MonadTimer m
      , MonadMask  m
      , MonadThrow (STM m)
      , MonadSay   m
      )

instance Arbitrary DiffTime where
    arbitrary = millisecondsToDiffTime <$>
                frequency
                  [ (4, choose (0,  5))
                  , (1, choose (5, 10))
                  ]
      where
        millisecondsToDiffTime = picosecondsToDiffTime . (* 1_000_000_000)

    shrink = map (fromRational . getNonNegative)
           . shrink
           . NonNegative
           . toRational

singleTimeoutExperiment
    :: TimeoutConstraints m
    => TimeoutDuration
    -> ActionDuration
    -> m (WithSanityCheck Property)
singleTimeoutExperiment intendedTimeoutDuration
                        intendedActionDuration = do

    before <- getMonotonicTime

              -- Allow the action to run for intendedTimeoutDuration
    result <- timeout intendedTimeoutDuration $ do

                  getMaskingState >>= say . show
                  -- Simulate an action that should take intendedActionDuration
                  threadDelay intendedActionDuration

                  -- but we also measure the actual duration
                  getMonotonicTime

    after  <- getMonotonicTime

    return $ experimentResult intendedTimeoutDuration
                              intendedActionDuration
                              before after result

data WithSanityCheck prop
  = WithSanityCheck        prop

  -- | The first one represents the property without sanity check, the other one
  -- sanity check (which failed). It is kept to keep its `counterexample`s.
  | WithSanityCheckFailure prop prop
  deriving (Functor)

ignoreSanityCheck :: WithSanityCheck prop -> prop
ignoreSanityCheck (WithSanityCheck    prop)       = prop
ignoreSanityCheck (WithSanityCheckFailure prop _) = prop

withSanityCheck :: WithSanityCheck Property -> Property
withSanityCheck (WithSanityCheck        prop)             = prop
withSanityCheck (WithSanityCheckFailure prop sanityCheck) = prop .&&. sanityCheck

isSanityCheckIgnored :: WithSanityCheck prop -> Bool
isSanityCheckIgnored WithSanityCheck{}         = False
isSanityCheckIgnored WithSanityCheckFailure {} = True


experimentResult :: TimeoutDuration
                 -> ActionDuration
                 -> Time
                 -> Time
                 -> Maybe Time
                 -> WithSanityCheck Property
experimentResult intendedTimeoutDuration
                 intendedActionDuration
                 before after result =
    counterexamples
      [ "intendedTimeoutDuration: " ++ show intendedTimeoutDuration
      , "intendedActionDuration:  " ++ show intendedActionDuration
      , "actualOverallDuration:   " ++ show actualOverallDuration
      ] <$>
      if ignoredSanityCheck
        then WithSanityCheckFailure timeoutCheck sanityCheck
        else WithSanityCheck $ sanityCheck .&&. timeoutCheck
  where
    actualOverallDuration   = diffTime after before
    intendedOverallDuration = min intendedTimeoutDuration intendedActionDuration

    ignoredSanityCheck =
         actualOverallDuration < intendedOverallDuration
      || actualOverallDuration > intendedOverallDuration

    sanityCheck = counterexample "sanityCheckLow"  sanityCheckLow
             .&&. counterexample "sanityCheckHigh" sanityCheckHigh

    sanityCheckLow =
      actualOverallDuration >= intendedOverallDuration

    sanityCheckHigh =
      actualOverallDuration <= intendedOverallDuration

    timeoutCheck =
      case result of
        Nothing ->
          counterexamples
            [ "timeout fired (but should not have)"
            , "violation of timeout property:\n" ++
              "  actualOverallDuration >= intendedTimeoutDuration"
            ] $
          actualOverallDuration >= intendedTimeoutDuration

        Just afterAction ->
          let actualActionDuration = diffTime afterAction before in
          counterexamples
            [ "actualActionDuration:  " ++ show actualActionDuration
            , "timeout did not fire (but should not have)"
            , "violation of timeout property:\n" ++
              "  actualActionDuration <= intendedTimeoutDuration"
            ] $
          actualActionDuration <= intendedActionDuration


prop_timeout
    :: TimeoutDuration
    -> ActionDuration
    -> Property
prop_timeout intendedTimeoutDuration intendedActionDuration =
    runSimOrThrow (withSanityCheck <$>
                    singleTimeoutExperiment
                      intendedTimeoutDuration
                      intendedActionDuration)


prop_timeouts
    :: [(TimeoutDuration, ActionDuration)]
    -> Property
prop_timeouts times =
    counterexample (ppTrace_ trace) $
    either (\e -> counterexample (show e) False) id $
    traceResult False trace
  where
    trace =
      runSimTrace $
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


prop_stacked_timeouts :: TimeoutDuration
                      -> TimeoutDuration
                      -> ActionDuration
                      -> Property
prop_stacked_timeouts timeout0 timeout1 actionDuration =
    let trace = runSimTrace experiment in
    counterexample (ppTrace_ trace) $
    either (\e -> counterexample (show e) False) (=== predicted) (traceResult False trace)
  where
    experiment :: IOSim s (Maybe (Maybe ()))
    experiment = timeout timeout0 (timeout timeout1 (threadDelay actionDuration))

    predicted | timeout0 == 0
              = Nothing

              | timeout1 == 0
              = Just Nothing

              | actionDuration <= min timeout0 timeout1
              = Just (Just ())

              | timeout0 < timeout1
              = Nothing

              | otherwise -- i.e. timeout0 >= timeout1
              = Just Nothing


-- | Check that `discardAfter` works as expected.
--
-- NOTE: using `discardAfter` with `IOSim` is more tricky than for `IO`
-- properties, since `IOSim` is a pure computation.  One need to wrap the
-- simulation in a lambda and use `-fno-full-laziness` to avoid GHC from
-- moving the thunk outside of the lambda, and evaluating it just once.
--
unit_discardAfter :: Property
unit_discardAfter = once
                  . mapTotalResult f
                  . discardAfter 10
                  $ \() -> runSimOrThrow $ True <$ (forever (threadDelay 10))
  where
    -- if `discard` kills the computation with the `Timeout` exception,
    -- `theException` is `Nothing`, but if `traceResult` wraps it, then it is
    -- a `Just`. We mark each test a success if `theException` is `Nothing`,
    -- otherwise the test would fail with too many discarded cases, but if we re
    -- introduce the bug in `traceResult` then it fails, since then
    -- `theException` is a `Just`.
    f :: QC.Result -> QC.Result
    f r@MkResult { QC.theException = Nothing }
        = r { ok = Just True }
    f r = r


-- | Check that `within` works as expected.
--
unit_within :: Property
unit_within = once
            . mapTotalResult f
            . within 10
            $ runSimOrThrow $ True <$ (forever (threadDelay 10))
  where
    -- if `within` kills the computation with the `Timeout` exception,
    -- `theException` is `Nothing`, but if `traceResult` wraps it, then it is
    -- a `Just`.
    f :: QC.Result -> QC.Result
    f r@MkResult { QC.theException = Nothing }
        = r { expect = False }
    f r = r


unit_timeouts_and_async_exceptions_1 :: Property
unit_timeouts_and_async_exceptions_1 = once $
    let trace = runSimTrace experiment in
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      tid <- forkIO $ void $
        timeout delay (atomically retry)

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True


unit_timeouts_and_async_exceptions_2 :: Property
unit_timeouts_and_async_exceptions_2 = once $
    let trace = runSimTrace experiment in
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      tid <- forkIO $ void $
        timeout delay (atomically retry) `catch` (\(_ :: AsyncException) -> return Nothing)

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True


unit_timeouts_and_async_exceptions_3 :: Property
unit_timeouts_and_async_exceptions_3 = once $
    let trace = runSimTrace experiment in
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    delay = 1

    experiment :: IOSim s Property
    experiment = do
      tid <- forkIO $ void $
        timeout delay (atomically retry `catch` (\(_ :: AsyncException) -> return ()))

      threadDelay (delay / 2)
      killThread tid
      threadDelay 1
      return $ property True


-- | Verify that a thread blocked on `threadDelay` is not unblocked by an STM
-- transaction.
--
unit_threadDelay_and_stm :: Property
unit_threadDelay_and_stm = once $
    let trace = runSimTrace experiment in
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    experiment :: IOSim s Property
    experiment = do
      v0 <- newTVarIO False
      v1 <- newTVarIO False

      _ <- forkIO $ do
        threadDelay 1
        atomically $ writeTVar v0 True
      atomically $ (readTVar v1 >>= check) `orElse` (readTVar v0 >>= check)

      let delay = 2
      t0 <- getMonotonicTime
      _ <- forkIO $ do
        threadDelay 1
        atomically $ writeTVar v1 True
      threadDelay delay
      t1 <- getMonotonicTime

      return (t1 `diffTime` t0 === delay)

unit_registerDelay_threadDelay :: Property
unit_registerDelay_threadDelay = once $
    let trace = runSimTrace experiment in
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
        b0 <- LazySTM.readTVar v0
        b1 <- readTVar v1
        check $ b0 || b1

      let delay = 2
      t0 <- getMonotonicTime
      threadDelay delay
      t1 <- getMonotonicTime

      return (t1 `diffTime` t0 === delay)

-- | Verify that a thread blocked on `throwTo` is not unblocked by an STM
-- transaction.
--
unit_throwTo_and_stm :: Property
unit_throwTo_and_stm = once $
    let trace = runSimTrace experiment in
        counterexample (ppTrace_ trace)
      . either (\e -> counterexample (show e) False) id
      . traceResult False
      $ trace
  where
    experiment :: IOSim s Property
    experiment = do
      v0 <- newTVarIO False
      v1 <- newTVarIO False

      _ <- forkIO $ do
        threadDelay 1
        atomically $ writeTVar v0 True
      atomically $ (readTVar v1 >>= check) `orElse` (readTVar v0 >>= check)

      let delay = 2
      t0 <- getMonotonicTime
      _ <- forkIO $ do
        threadDelay 1
        atomically $ writeTVar v1 True
      tid <- forkIO $ uninterruptibleMask_ (threadDelay 2)
      threadDelay 0.1 -- make sure the other thread masks exceptions
      killThread tid
      t1 <- getMonotonicTime

      return (t1 `diffTime` t0 === delay)

--
-- MonadMask properties
--

unit_set_masking_state_IO :: MaskingState -> Property
unit_set_masking_state_IO = once .
    ioProperty . prop_set_masking_state

unit_set_masking_state_ST :: MaskingState -> Property
unit_set_masking_state_ST ms = once $
    runSimOrThrow (prop_set_masking_state ms)

unit_unmask_IO :: MaskingState -> MaskingState -> Property
unit_unmask_IO ms ms' = once $ ioProperty $ prop_unmask ms ms'

unit_unmask_ST :: MaskingState -> MaskingState -> Property
unit_unmask_ST ms ms' = once $ runSimOrThrow $ prop_unmask ms ms'

unit_fork_masking_state_IO :: MaskingState -> Property
unit_fork_masking_state_IO = once .
    ioProperty . prop_fork_masking_state

unit_fork_masking_state_ST :: MaskingState -> Property
unit_fork_masking_state_ST ms = once $
    runSimOrThrow (prop_fork_masking_state ms)

unit_fork_unmask_IO :: MaskingState -> MaskingState -> Property
unit_fork_unmask_IO ms ms' = once $ ioProperty $ prop_fork_unmask ms ms'

unit_fork_unmask_ST :: MaskingState -> MaskingState -> Property
unit_fork_unmask_ST ms ms' = once $ runSimOrThrow $ prop_fork_unmask ms ms'

unit_catch_throwIO_masking_state_IO :: MaskingState -> Property
unit_catch_throwIO_masking_state_IO ms = once $
    ioProperty $ prop_catch_throwIO_masking_state ms

unit_catch_throwIO_masking_state_ST :: MaskingState -> Property
unit_catch_throwIO_masking_state_ST ms = once $
    runSimOrThrow (prop_catch_throwIO_masking_state ms)

unit_catch_throwTo_masking_state_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_IO = once .
    ioProperty . prop_catch_throwTo_masking_state

unit_catch_throwTo_masking_state_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_ST ms = once $
    runSimOrThrow $ prop_catch_throwTo_masking_state ms

unit_catch_throwTo_masking_state_async_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_IO = once .
    ioProperty . prop_catch_throwTo_masking_state_async

unit_catch_throwTo_masking_state_async_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_ST ms = once $
    runSimOrThrow (prop_catch_throwTo_masking_state_async ms)

unit_catch_throwTo_masking_state_async_mayblock_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_IO = once .
    ioProperty . prop_catch_throwTo_masking_state_async_mayblock

unit_catch_throwTo_masking_state_async_mayblock_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_ST ms = once $
    runSimOrThrow (prop_catch_throwTo_masking_state_async_mayblock ms)

--
-- MonadTimerCancellable
--

data DelayWithCancel = DelayWithCancel DiffTime (Maybe DiffTime)
  deriving Show

instance Arbitrary DelayWithCancel where
  arbitrary =
      oneof
        [ -- small delay
          (\delay -> DelayWithCancel
                         (microsecondsAsIntToDiffTime delay)
                         Nothing)
              <$> arbitrary
          -- cancelled delay after small delay
        , (\delay -> DelayWithCancel
                         (microsecondsAsIntToDiffTime delay + maxDelay)
                         Nothing)
              <$> arbitrary

        , -- large delay
          do delay <- arbitrary
             cancel_ <- arbitrary `suchThat` (<= delay)
             return (DelayWithCancel (microsecondsAsIntToDiffTime delay)
                               (Just (microsecondsAsIntToDiffTime cancel_)))
        , -- cancelled delay after large delay
          do delay <- arbitrary
             cancel_ <- arbitrary `suchThat` (<= delay)
             return (DelayWithCancel (microsecondsAsIntToDiffTime delay + maxDelay)
                               (Just (microsecondsAsIntToDiffTime cancel_ + maxDelay)))
        ]
    where
      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

prop_registerDelayCancellable
    :: (forall s. DiffTime -> IOSim s (STM (IOSim s) TimeoutState, IOSim s ()))
    -- ^ implementation
    -> DelayWithCancel
    -> Property
prop_registerDelayCancellable registerDelayCancellableImpl
                              (DelayWithCancel delay mbCancel) =
      -- 'within' covers the case where `registerDelayCancellable` would not
      -- make progress awaiting for the timeout (a live lock).
      within 50_000 $ -- 50ms
      let trace = runSimTrace sim
      in case traceResult True trace of
        Left  err    -> counterexample (ppTrace trace)
                      . counterexample (show err)
                      $ False
        Right (_, r) -> counterexample (ppTrace trace) r
    where
      sim :: IOSim s (Maybe TimeoutState, Bool)
      sim = do
        (readTimeout_, cancelTimeout_) <- registerDelayCancellableImpl delay
        case mbCancel of

          Nothing -> do
            atomically $ do
              tv <- readTimeout_
              case tv of
                TimeoutFired     -> return ()
                TimeoutPending   -> retry
                TimeoutCancelled -> return ()
            t <- getMonotonicTime
            return (Nothing, t == Time (delay `max` 0))

          Just cancelDelay -> do
            threadDelay cancelDelay
            cancelTimeout_
            tv <- atomically readTimeout_
            return $ case () of
              _ | delay < cancelDelay  -> (Just tv, tv == TimeoutFired)
                | delay == cancelDelay -> (Just tv, tv == TimeoutFired
                                                 || tv == TimeoutCancelled)
                | otherwise            -> (Just tv, tv == TimeoutCancelled)



-- | Both tests run in `IOSim`, they only differ with the implementation of
-- `registerDelayCancellable`
--
prop_registerDelayCancellable_IOSim, prop_registerDelayCancellable_IO
    :: DelayWithCancel -> Property

prop_registerDelayCancellable_IOSim =
    prop_registerDelayCancellable registerDelayCancellable

prop_registerDelayCancellable_IO =
    prop_registerDelayCancellable  $
      defaultRegisterDelayCancellable
        (newTimeout . microsecondsAsIntToDiffTime)
        readTimeout
        cancelTimeout
        awaitTimeout

-- | Test that 'flushTQueue' empties the queue in `IO`.
prop_flushTQueueEmpties_IO :: Property
prop_flushTQueueEmpties_IO =
  ioProperty emptyTQueueAfterFlush

-- | Test that 'flushTQueue' empties the queue in `IOSim`.
prop_flushTQueueEmpties_IOSim :: Bool
prop_flushTQueueEmpties_IOSim =
  runSimOrThrow emptyTQueueAfterFlush

emptyTQueueAfterFlush :: MonadSTM m => m Bool
emptyTQueueAfterFlush = do
  q <- newTQueueIO
  atomically $ do
    writeTQueue q (1 :: Int)
    _ <- flushTQueue q
    isEmptyTQueue q

-- | Test that 'flushTQueue' returns values in FIFO order in `IO`.
prop_flushTQueueOrder_IO :: [Int] -> Property
prop_flushTQueueOrder_IO entries =
  ioProperty (writeAndFlushTQueue entries >>= \actual -> pure $ actual === entries)

-- | Test that 'flushTQueue' returns values in FIFO order in `IOSim`.
prop_flushTQueueOrder_IOSim :: [Int] -> Property
prop_flushTQueueOrder_IOSim entries =
  runSimOrThrow (writeAndFlushTQueue entries) === entries

writeAndFlushTQueue :: MonadSTM m => [Int] -> m [Int]
writeAndFlushTQueue entries =
  atomically $ do
    q <- newTQueue
    forM_ entries $ writeTQueue q
    flushTQueue q

-- | Test that 'flushTBQueue' empties the queue in `IO`.
prop_flushTBQueueEmpties_IO :: Property
prop_flushTBQueueEmpties_IO =
  ioProperty emptyTBQueueAfterFlush

-- | Test that 'flushTBQueue' empties the queue in `IOSim`.
prop_flushTBQueueEmpties_IOSim :: Bool
prop_flushTBQueueEmpties_IOSim =
  runSimOrThrow emptyTBQueueAfterFlush

emptyTBQueueAfterFlush :: MonadSTM m => m Bool
emptyTBQueueAfterFlush = do
  q <- newTBQueueIO 10
  atomically $ do
    writeTBQueue q (1 :: Int)
    writeTBQueue q 2
    _ <- flushTBQueue q
    isEmptyTBQueue q

-- | Test that 'flushTBQueue' returns values in FIFO order in `IO`.
prop_flushTBQueueOrder_IO :: [Int] -> Property
prop_flushTBQueueOrder_IO entries =
  ioProperty (writeAndFlushTBQueue entries >>= \actual -> pure $ actual === entries)

-- | Test that 'flushTBQueue' returns values in FIFO order in `IOSim`.
prop_flushTBQueueOrder_IOSim :: [Int] -> Property
prop_flushTBQueueOrder_IOSim entries =
  runSimOrThrow (writeAndFlushTBQueue entries) === entries

writeAndFlushTBQueue :: MonadSTM m => [Int] -> m [Int]
writeAndFlushTBQueue entries =
  atomically $ do
    q <- newTBQueue (1 + fromIntegral (length entries))
    forM_ entries $ writeTBQueue q
    flushTBQueue q

prop_tryReadEmptyTBQueue_IO :: Bool -> Property
prop_tryReadEmptyTBQueue_IO sndRead =
  ioProperty $ tryReadEmptyTBQueue sndRead

prop_tryReadEmptyTBQueue_IOSim :: Bool -> Property
prop_tryReadEmptyTBQueue_IOSim sndRead =
  runSimOrThrow $ tryReadEmptyTBQueue sndRead

tryReadEmptyTBQueue :: MonadSTM m => Bool -> m Property
tryReadEmptyTBQueue sndRead = atomically $ do
  q <- newTBQueue 10
  _ <- tryReadTBQueue q
  writeTBQueue q ()
  when sndRead $ void $ tryReadTBQueue q
  l <- lengthTBQueue q

  pure $ l === if sndRead then 0 else 1

--
-- Utils
--

counterexamples :: Testable t => [String] -> t -> Property
counterexamples []     p = property p
counterexamples (c:cs) p = counterexample c (counterexamples cs p)
