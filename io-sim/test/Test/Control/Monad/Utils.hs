{-# LANGUAGE CPP #-}
module Test.Control.Monad.Utils where

import Data.Array
import Data.Either (isLeft)
import Data.Fixed (Fixed (..), Micro)
import Data.Function (on)
import Data.Graph
import Data.List (sortBy)

import Control.Exception (AsyncException, ErrorCall (..))
import Control.Monad

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim

import Test.Control.Monad.STM

import Test.QuickCheck

#if !MIN_VERSION_QuickCheck(2,18,0)
withNumTests :: Testable prop => Int -> prop -> Property
withNumTests = withMaxSuccess
#endif

--
-- Read/Write graph
--

prop_stm_graph :: (MonadFork m, MonadSTM m) => TestThreadGraph -> m ()
prop_stm_graph (TestThreadGraph g) = do
    vars <- listArray (bounds g) <$>
            sequence [ newTVarIO False | _ <- vertices g ]
    forM_ (vertices g) $ \v ->
      void $ forkIO $ do
        -- read all the inputs and wait for them to become true
        -- then write to all the outputs
        let incomming = g' ! v
            outgoing  = g  ! v
        atomically $ do
          sequence_ [ readTVar  (vars ! var) >>= check | var <- incomming ]
          sequence_ [ writeTVar (vars ! var) True      | var <- outgoing  ]

    let -- the vertices with outgoing but no incoming edges
        inputs  = [ v
                  | v <- vertices g
                  , not (null (g  ! v))
                  ,      null (g' ! v) ]
        -- the vertices with incoming but no outgoing edges
        outputs = [ v
                  | v <- vertices g
                  , not (null (g' ! v))
                  ,      null (g  ! v) ]

    -- write to the inputs and wait for the outputs
    void $ forkIO $ atomically $ sequence_ [ writeTVar (vars ! var) True | var <- inputs  ]
    atomically $ sequence_ [ readTVar (vars ! var) >>= check | var <- outputs ]
  where
    g' = transposeG g -- for incoming edges

newtype TestThreadGraph = TestThreadGraph Graph
  deriving Show

instance Arbitrary TestThreadGraph where
  arbitrary =
    sized $ \sz ->
    TestThreadGraph <$> arbitraryAcyclicGraph
                          (choose (2, 8 `min` (sz `div` 3)))
                          (choose (1, 8 `min` (sz `div` 3)))
                          0.3

arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

    pick :: Float -> Gen Bool
    pick chance = (< chance) <$> choose (0,1)


--
-- Timers
--

newtype TestMicro = TestMicro [Micro]
  deriving Show

-- |
-- Arbitrary non negative micro numbers with a high probability of
-- repetitions.
instance Arbitrary TestMicro where
  arbitrary = sized $ \n -> TestMicro <$> genN n []
    where
      genN :: Int -> [Micro] -> Gen [Micro]
      genN 0 rs = return rs
      genN n [] = do
        r <- genMicro
        genN (n - 1) [r]
      genN n rs = do
        r <- frequency
          [ (2, elements rs)
          , (1, genMicro)
          ]
        genN (n - 1) (r : rs)

      genMicro :: Gen Micro
      genMicro = MkFixed <$> arbitrary

  shrink (TestMicro rs) = [ TestMicro rs' | rs' <- shrinkList (const []) rs ]

test_timers :: forall m.
               ( MonadDelay m
               , MonadFork  m
               , MonadTimer m
               )
            => [DiffTime]
            -> m Property
test_timers xs =
    label (lbl xs) . isValid <$> withProbe experiment
  where
    countUnique :: Eq a => [a] -> Int
    countUnique [] = 0
    countUnique (a:as) =
      let as' = filter (== a) as
      in 1 + countUnique as'

    lbl :: Eq a => [a] -> String
    lbl as =
      let p = (if null as then 0 else (100 * countUnique as) `div` length as) `mod` 10 * 10
      in show p ++ "% unique"

    experiment :: Probe m (DiffTime, Int) -> m ()
    experiment p = do
      tvars <- forM (zip xs [0..]) $ \(t, idx) -> do
        v <- newTVarIO False
        void $ forkIO $ threadDelay t >> do
          probeOutput p (t, idx)
          atomically $ writeTVar v True
        return v

      -- wait for all tvars
      forM_ tvars $ \v -> atomically (readTVar v >>= check)

    isValid :: [(DiffTime, Int)] -> Property
    isValid tr =
         -- all timers should fire
         (length tr === length xs)
         -- timers should fire in the right order
      .&&. (sortBy (on sortFn fst) tr === tr)

    -- timers with negative timeout never fired, so we treat them as they would
    -- all fired at once at @-∞@.  This is to say that the following function is
    -- a well defined partial order.
    sortFn :: DiffTime -> DiffTime -> Ordering
    sortFn a b | a >= 0 && b >= 0 = a `compare` b
               | a  < 0 && b  < 0 = EQ
               | otherwise = a `compare` b

--
-- Forking
--

test_fork_order :: forall m.
                   ( MonadFork  m
                   , MonadTimer m
                   )
                => Positive Int
                -> m Property
test_fork_order = \(Positive n) -> isValid n <$> withProbe (experiment n)
  where
    experiment :: Int -> Probe m Int -> m ()
    experiment 0 _ = return ()
    experiment n p = do
      v <- newTVarIO False

      void $ forkIO $ do
        probeOutput p n
        atomically $ writeTVar v True
      experiment (n - 1) p

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check

    isValid :: Int -> [Int] -> Property
    isValid n tr = tr === [n,n-1..1]

test_threadId_order :: forall m.
                       ( MonadFork  m
                       , MonadTimer m
                       )
                    => Positive Int
                    -> m Property
test_threadId_order = \(Positive n) -> do
    isValid n <$> (forM [1..n] (const experiment))
  where
    experiment :: m (ThreadId m)
    experiment = do
      v <- newTVarIO False

      tid <- forkIO $ atomically $ writeTVar v True

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check
      return tid

    isValid :: Int -> [ThreadId m] -> Property
    isValid n tr = map show tr === map (("ThreadId " ++ ) . show . (:[])) [1..n]

-- This property is not actually deterministic in IO. Uncomment the following
-- and try it! Arguably therefore, this property does not need to be true for
-- the Sim either. Perhaps we should introduce random scheduling and abandon
-- this property. In the meantime it's a helpful sanity check.

--prop_wakeup_order_IO = ioProperty test_wakeup_order

test_wakeup_order :: ( MonadDelay m
                     , MonadFork  m
                     , MonadTimer m
                     )
                => m Property
test_wakeup_order = do
    v          <- newTVarIO False
    wakupOrder <-
      withProbe $ \p -> do
        sequence_
          [ do _ <- forkIO $ do
                 atomically $ do
                   x <- readTVar v
                   check x
                 probeOutput p (n :: Int)
               threadDelay 0.1
          | n <- [0..9] ]
        atomically $ writeTVar v True
        threadDelay 0.1
    return (wakupOrder === [0..9]) --FIFO order

--
-- Probe mini-abstraction
--

-- | Where returning results directly is not convenient, we can build up
-- a trace of events we want to observe, and can do probe output from
-- multiple threads.
--
type Probe m x = StrictTVar m [x]

withProbe :: MonadSTM m => (Probe m x -> m ()) -> m [x]
withProbe action = do
    probe <- newTVarIO []
    action probe
    reverse <$> atomically (readTVar probe)

probeOutput :: MonadSTM m => Probe m x -> x -> m ()
probeOutput probe x = atomically (modifyTVar probe (x:))

--
-- Tests vs STM operational semantics
--

--TODO: would be nice to also have stronger tests here:
-- * compare all the tvar values in the heap
-- * compare the read and write sets

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of any 'MonadSTM' STM implementation.
--
prop_stm_referenceM :: ( MonadSTM m
                       , MonadCatch (STM m)
                       , MonadCatch m
                       )
                    => SomeTerm -> m Property
prop_stm_referenceM (SomeTerm _tyrep t) = do
    let (r1, _heap) = evalAtomically t
    r2 <- execAtomically t
    return (r1 === r2)

-- | Check that 'timeout' does not deadlock when executed with asynchronous
-- exceptions uninterruptibly masked.
--
prop_timeout_no_deadlockM :: forall m.
                             ( MonadDelay m
                             , MonadFork  m
                             , MonadTimer m
                             , MonadMask  m
                             )
                          => m Bool
prop_timeout_no_deadlockM = do
    v <- registerDelay' 0.01
    r <- uninterruptibleMask_ $ timeout 0.02 $ do
      atomically $ do
        readTVar v >>= check
        return True
    case r of
      Nothing -> return False
      Just b  -> return b
  where
    -- Like 'registerDelay', but does not require threaded RTS in the @m ~ IO@
    -- case.
    registerDelay' :: DiffTime -> m (StrictTVar m Bool)
    registerDelay' delta = do
      v <- newTVarIO False
      _ <- forkIO $ do
             threadDelay delta
             atomically (writeTVar v True)
      return v

--
-- MonadMask properties
--

setMaskingState_ :: MonadMask m => MaskingState -> m a -> m a
setMaskingState_ Unmasked              = id
setMaskingState_ MaskedInterruptible   = mask_
setMaskingState_ MaskedUninterruptible = uninterruptibleMask_

setMaskingState :: MonadMask m => MaskingState
                -> ((forall x. m x -> m x) -> m a) -> m a
setMaskingState Unmasked              = \f -> f id
setMaskingState MaskedInterruptible   = mask
setMaskingState MaskedUninterruptible = uninterruptibleMask

maxMS :: MaskingState -> MaskingState -> MaskingState
maxMS MaskedUninterruptible _                     = MaskedUninterruptible
maxMS _                     MaskedUninterruptible = MaskedUninterruptible
maxMS MaskedInterruptible   _                     = MaskedInterruptible
maxMS _                     MaskedInterruptible   = MaskedInterruptible
maxMS Unmasked              Unmasked              = Unmasked

-- | Check that setting masking state is effective.
--
prop_set_masking_state :: MonadMask m
                       => MaskingState
                       -> m Property
prop_set_masking_state ms =
    setMaskingState_ ms $ do
      ms' <- getMaskingState
      return (ms === ms')

-- | Check that 'unmask' restores the masking state.
--
prop_unmask :: MonadMask m
            => MaskingState
            -> MaskingState
            -> m Property
prop_unmask ms ms' =
    setMaskingState_ ms $
      setMaskingState ms' $ \unmask -> do
        ms'' <- unmask getMaskingState
        return (ms'' === ms)

-- | Check that masking state is inherited by a forked thread.
--
prop_fork_masking_state :: ( MonadMask m
                           , MonadFork m
                           , MonadSTM m
                           )
                        => MaskingState -> m Property
prop_fork_masking_state ms = setMaskingState_ ms $ do
    var <- newEmptyTMVarIO
    _ <- forkIO $ getMaskingState >>= atomically . putTMVar var
    ms' <- atomically $ takeTMVar var
    return $ ms === ms'

-- | Check that 'unmask' restores the masking state in a forked thread.
--
-- Note: unlike 'prop_unmask', 'forkIOWithUnmask's 'unmask' function will
-- restore 'Unmasked' state, not the encosing masking state.
--
prop_fork_unmask :: ( MonadMask m
                    , MonadFork m
                    , MonadSTM m
                    )
                 => MaskingState
                 -> MaskingState
                 -> m Property
prop_fork_unmask ms ms' =
    setMaskingState_ ms $
      setMaskingState_ ms' $ do
        var <- newEmptyTMVarIO
        _ <- forkIOWithUnmask $ \unmask -> unmask getMaskingState
                                       >>= atomically . putTMVar var
        ms'' <- atomically $ takeTMVar var
        return $ Unmasked === ms''

-- | A unit test which checks the masking state in the context of a catch
-- handler.
--
prop_catch_throwIO_masking_state :: forall m. MonadMask m
                                 => MaskingState
                                 -> m Property
prop_catch_throwIO_masking_state ms =
    setMaskingState_ ms $ do
      throwIO (userError "error")
      `catch` \(_ :: IOError) -> do
        ms' <- getMaskingState
        return $ ms' === MaskedInterruptible `maxMS` ms

-- | The masking state is restored when an exception, thrown under a different
-- masking state, is caught.  The exception is thrown under @ms'@: @restore@
-- unmasks first, since @'setMaskingState_' 'Unmasked'@ does not unmask.
--
prop_catch_restores_masking_state :: forall m. MonadMask m
                                  => MaskingState
                                  -> MaskingState
                                  -> m Property
prop_catch_restores_masking_state ms ms' =
    setMaskingState ms $ \restore -> do
      _ <- try @_ @IOError $
             restore (setMaskingState_ ms' (throwIO (userError "error")))
      ms'' <- getMaskingState
      return $ ms'' === ms

-- | The masking state is restored when 'timeout' fires, whatever the masking
-- state of the timed action.  The action runs under @ms'@ (see
-- 'prop_catch_restores_masking_state'), and the timeout is set to fire before
-- it finishes.
--
prop_timeout_restores_masking_state :: forall m. ( MonadMask  m
                                                 , MonadDelay m
                                                 , MonadTimer m
                                                 )
                                    => MaskingState
                                    -> MaskingState
                                    -> m Property
prop_timeout_restores_masking_state ms ms' =
    setMaskingState ms $ \restore -> do
      -- timeout before the action finish
      _ <- timeout 0.01 (restore (setMaskingState_ ms' (threadDelay 0.05)))
      ms'' <- getMaskingState
      return $ ms'' === ms

-- | Like 'prop_catch_masking_state' but using 'throwTo'.
--
prop_catch_throwTo_masking_state :: forall m.
                                    ( MonadMask m
                                    , MonadFork m
                                    )
                                 => MaskingState -> m Property
prop_catch_throwTo_masking_state ms =
    setMaskingState_ ms $ do
      tid <- myThreadId
      (throwTo tid (userError "error") >> error "impossible")
      `catch` \(_ :: IOError) -> do
        ms' <- getMaskingState
        return $ ms' === MaskedInterruptible `maxMS` ms

-- | Like 'prop_catch_throwTo_masking_state' but using 'throwTo' to a different
-- thread which is in a non-blocking mode.
--
prop_catch_throwTo_masking_state_async :: forall m.
                                          ( MonadMask  m
                                          , MonadFork  m
                                          , MonadSTM   m
                                          , MonadDelay m
                                          )
                                       => MaskingState -> m Property
prop_catch_throwTo_masking_state_async ms = do
    sgnl <- newEmptyTMVarIO
    var <- newEmptyTMVarIO
    tid <- forkIO $
      setMaskingState ms $ \unmask ->
        (do atomically $ putTMVar sgnl ()
            unmask (threadDelay 1)
        )
        `catch` \(_ :: IOError) -> do
          ms' <- getMaskingState
          atomically $ putTMVar var (ms' === ms `maxMS` MaskedInterruptible)
    -- wait until the catch handler is installed
    atomically $ takeTMVar sgnl
    -- the forked thread is interruptibly blocked on `threadDelay`,
    -- `throwTo` will not block
    throwTo tid (userError "error")
    atomically $ takeTMVar var

-- | Like 'prop_catch_throwTo_masking_state_async' but 'throwTo' will block if
-- masking state is set to 'MaskedUninterruptible'.  This makes sure that the
-- 'willBlock' branch of 'ThrowTo' in 'schedule' is covered.
--
prop_catch_throwTo_masking_state_async_mayblock :: forall m.
                                                ( MonadMask  m
                                                , MonadFork  m
                                                , MonadSTM   m
                                                , MonadDelay m
                                                )
                                             => MaskingState -> m Property
prop_catch_throwTo_masking_state_async_mayblock ms = do
    sgnl <- newEmptyTMVarIO
    var <- newEmptyTMVarIO
    tid <- forkIO $
      setMaskingState ms $ \unmask ->
        (do atomically $ putTMVar sgnl ()
            -- if 'ms' is 'MaskedUninterruptible' then the following
            -- 'threadDelay' will block.
            threadDelay 0.1
            -- make sure that even in 'MaskedUninterruptible' the thread
            -- unblocks so async exceptions can be delivered.
            unmask (threadDelay 1)
        )
        `catch` \(_ :: IOError) -> do
          ms' <- getMaskingState
          atomically $ putTMVar var (ms' === ms `maxMS` MaskedInterruptible)
    -- wait until the catch handler is installed
    atomically $ takeTMVar sgnl
    threadDelay 0.05
    -- we know the forked thread is interruptibly blocked on `threadDelay`,
    -- `throwTo` will not be blocked.
    throwTo tid (userError "error")
    atomically $ takeTMVar var

--
-- MonadMask properties
--

forall_masking_states :: (MaskingState -> Property)
                      -> Property
forall_masking_states prop =
    -- make sure that the property is executed once!
    withNumTests 1 $
    foldr (\ms p -> counterexample (show ms) (prop ms) .&&. p)
          (property True)
          [Unmasked, MaskedInterruptible, MaskedUninterruptible]

-- | Like `forall_masking_states`, but only for the masked states.
--
forall_masked_states :: (MaskingState -> Property)
                     -> Property
forall_masked_states prop =
    -- make sure that the property is executed once!
    withNumTests 1 $
    foldr (\ms p -> counterexample (show ms) (prop ms) .&&. p)
          (property True)
          [MaskedInterruptible, MaskedUninterruptible]

--
-- Utils
--

runSimTraceSay :: (forall s. IOSim s a) -> [String]
runSimTraceSay action = selectTraceSay (runSimTrace action)

selectTraceSay :: SimTrace a -> [String]
selectTraceSay (SimTrace _ _ _ (EventSay msg) trace)      = msg : selectTraceSay trace
selectTraceSay (SimTrace _ _ _ _              trace)      = selectTraceSay trace
selectTraceSay (SimPORTrace _ _ _ _ (EventSay msg) trace) = msg : selectTraceSay trace
selectTraceSay (SimPORTrace _ _ _ _ _              trace) = selectTraceSay trace
selectTraceSay  _                                         = []


--
-- `throwTo` and pending async exceptions
--

-- | A `throwTo` must not replace an async exception which was raised in its
-- target, but which the target has not run yet.
--
-- `killThread` is synchronous: `t` dies of `ThreadKilled` (which
-- @'try' \@ErrorCall@ does not catch), and the second `throwTo` finds it
-- dead.
--
prop_throwTo_does_not_overwrite_pending_exception
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadSTM   m
               )
  => m Property
prop_throwTo_does_not_overwrite_pending_exception = do
    res <- newTVarIO Nothing
    t <- forkIO $ do
           r <- try @_ @ErrorCall (threadDelay 1)
           atomically (writeTVar res (Just r))
    -- let `t` block in `threadDelay`
    threadDelay 0.01
    killThread t
    throwTo t (ErrorCall "second")
    threadDelay 0.1
    r <- readTVarIO res
    return $ counterexample "`t` survived" $ r === Nothing


--
-- Pending async exceptions
--

-- | When `timeout` returns unmasked, a pending async exception is raised
-- straight away: the code after `timeout` does not run.
--
-- @C@'s exception, and then the timeout's, are queued while the timed action
-- is uninterruptibly masked.  The timed action decides where the timeout's
-- exception is raised.
--
timeout_raises_pending_exception
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadTimer m
               )
  => (DiffTime -> m ())
  -- ^ timed action
  -> m Property
timeout_raises_pending_exception action = do
    me <- myThreadId
    after <- newTVarIO False
    _ <- forkIO $ threadDelay t0 >> throwTo me (ErrorCall "from C")
    r <- try @_ @ErrorCall $ do
           _ <- timeout t1 (action t1)
           -- `C`'s exception must be raised before this runs
           atomically (writeTVar after True)
           threadDelay t1
    ran <- readTVarIO after
    return $ r === Left (ErrorCall "from C")
        .&&. counterexample "the code after `timeout` ran" (not ran)
  where
    t0 = 0.05
    t1 = 2 * t0

-- | See 'timeout_raises_pending_exception': the timeout's exception is raised
-- at a blocking operation within `mask_`, and unwinds through its
-- 'MaskFrame', which restores 'Unmasked'.
--
prop_timeout_raises_pending_exception_at_blocking_op
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadTimer m
               )
  => m Property
prop_timeout_raises_pending_exception_at_blocking_op =
    timeout_raises_pending_exception
      (\t1 -> mask_ $   uninterruptibleMask_ (threadDelay (t1 + 0.1))
                     >> threadDelay t1)

-- | Two stacked timeouts.  The outer one catches its own exception after it
-- unwound the inner `TimeoutFrame`, whose assassin has thrown and still holds
-- its lock, while another async exception is pending.  The pending exception
-- is raised when `timeout` returns (see 'timeout_raises_pending_exception'),
-- and the inner assassin must still be killed: run with a strict shutdown
-- check, the simulation fails if it is left blocked in its `throwTo`.
--
-- @C@'s exception, and then the inner and the outer timeout's, are queued
-- while the timed action is uninterruptibly masked.  The outer timeout's
-- exception is raised first, and unwinds through the inner `TimeoutFrame`.
--
prop_timeout_stacked_pending_exception
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadTimer m
               )
  => m Property
prop_timeout_stacked_pending_exception = do
    me <- myThreadId
    _ <- forkIO $ threadDelay t0 >> throwTo me (ErrorCall "from C")
    r <- try @_ @ErrorCall $
           timeout t2 (timeout t1 (uninterruptibleMask_ (threadDelay t3)))
    -- let the other threads finish
    threadDelay t4
    return $ r === Left (ErrorCall "from C")

  where
    t0 = 0.05
    t1 = 0.1
    t2 = 0.15
    t3 = 0.3
    t4 = 0.35

-- | The timed action catches the timeout's exception, and another thread
-- forwards it back while the action is uninterruptibly masked.  Then the
-- action rethrows it, so it unwinds to its own `TimeoutFrame` while the
-- forwarded exception is pending.  Leaving the frame must not retract the
-- forwarded exception: it is raised after `timeout` returns, and the
-- forwarding thread's `throwTo` returns.  Otherwise the forwarding thread
-- stays blocked in `throwTo` forever, and the simulation deadlocks.
--
-- The forwarding thread is forked within `uninterruptibleMask_`, so in the
-- simulators its exception is pending when the action rethrows, whatever the
-- schedule.  In `IO` it might be thrown later, and then it is raised at the
-- interruptible point after `timeout`.
--
prop_timeout_forwarded_exception_rethrown
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadTimer m
               )
  => m Property
prop_timeout_forwarded_exception_rethrown = do
    me   <- myThreadId
    done <- newEmptyTMVarIO
    r <- try @_ @SomeException $ do
      _ <- timeout 0.05 $ do
        -- `threadDelay` is interruptible, the timeout fires here
        r' <- try @_ @SomeException (threadDelay 1)
        case r' of
          Left e   -> uninterruptibleMask_ $ do
            -- the forwarding thread blocks in `throwTo`
            _ <- forkIO $ throwTo me e
                       >> atomically (putTMVar done ())
            threadDelay 0.05
            -- unwinds to its own `TimeoutFrame`
            throwIO e
          Right () -> return ()
      -- an interruptible point
      atomically (takeTMVar done)
    -- the forwarding thread's `throwTo` returned
    atomically (takeTMVar done)
    return $ counterexample (show r) (isLeft r)

--
-- Cancelling `throwTo`
--

-- | A thread blocked in `throwTo` is interrupted by its own pending async
-- exception.  Its `throwTo` is cancelled, so the target never receives the
-- exception.
--
-- @A@ is masked, @C@'s exception is queued while @A@ is uninterruptibly
-- masked, then @A@ blocks in `throwTo` to @B@ (uninterruptibly masked until
-- t=2), which is an interruptible point.
--
prop_throwTo_interrupted_by_pending_exception
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadSTM   m
               )
  => m Property
prop_throwTo_interrupted_by_pending_exception = do
    resA <- newTVarIO Nothing
    resB <- newTVarIO Nothing
    b <- uninterruptibleMask_ $ forkIOWithUnmask $ \unmask -> do
           threadDelay 2
           -- a stale exception from `A` would be raised here
           r <- try @_ @ErrorCall (unmask (threadDelay 1))
           atomically (writeTVar resB (Just r))
    a <- mask_ $ forkIO $ do
           uninterruptibleMask_ (threadDelay 1)
           r <- try @_ @ErrorCall (throwTo b (ErrorCall "from A"))
           atomically (writeTVar resA (Just r))
    _ <- forkIO $ threadDelay 0.5 >> throwTo a (ErrorCall "from C")
    threadDelay 6
    ra <- readTVarIO resA
    rb <- readTVarIO resB
    return $ ra === Just (Left (ErrorCall "from C"))
        .&&. rb === Just (Right ())

-- | A thread blocked in `throwTo` is killed.  Its `throwTo` is cancelled, so
-- the target never receives the exception.
--
prop_throwTo_thrower_killed
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadSTM   m
               )
  => m Property
prop_throwTo_thrower_killed = do
    resB <- newTVarIO Nothing
    b <- uninterruptibleMask_ $ forkIOWithUnmask $ \unmask -> do
           threadDelay 2
           -- a stale exception from `A` would be raised here
           r <- try @_ @ErrorCall (unmask (threadDelay 1))
           atomically (writeTVar resB (Just r))
    a <- forkIO $ throwTo b (ErrorCall "from A")
    threadDelay 1
    killThread a
    threadDelay 5
    rb <- readTVarIO resB
    return $ rb === Just (Right ())

-- | A thread blocked in `throwTo` is killed, and then blocks in `threadDelay`.
-- When the target unmasks, it must not wake the thread up.
--
prop_throwTo_cancelled_thrower_not_woken
  :: forall m. ( MonadDelay         m
               , MonadFork          m
               , MonadMask          m
               , MonadSTM           m
               )
  => m Property
prop_throwTo_cancelled_thrower_not_woken = do
    resA <- newTVarIO Nothing
    b <- uninterruptibleMask_ $ forkIOWithUnmask $ \unmask -> do
           threadDelay 2
           unmask (threadDelay 1)
    a <- forkIO $
           handle (\(_ :: AsyncException) -> do
                     t0 <- getMonotonicTime
                     threadDelay 100
                     t1 <- getMonotonicTime
                     atomically (writeTVar resA (Just (t1 `diffTime` t0))))
                  (throwTo b (ErrorCall "from A"))
    threadDelay 1
    killThread a
    d <- atomically $ readTVar resA >>= maybe retry return
    return $ counterexample (show d) (d >= 100)

-- | See 'timeout_raises_pending_exception': the timeout's exception is raised
-- when the action unmasks.
--
prop_timeout_raises_pending_exception_on_unmask
  :: forall m. ( MonadDelay m
               , MonadFork  m
               , MonadMask  m
               , MonadTimer m
               )
  => m Property
prop_timeout_raises_pending_exception_on_unmask =
    timeout_raises_pending_exception
      (uninterruptibleMask_ . threadDelay . (+0.1))
