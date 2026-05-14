{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Control.Monad.IOSim
  ( -- * Simulation monad
    IOSim
  , STMSim
    -- ** Run simulation
  , runSim
  , runSimOrThrow
  , runSimStrictShutdown
  , Failure (..)
  , runSimTrace
  , runSimTraceST
    -- *** QuickCheck Monadic combinators
  , monadicIOSim_
  , monadicIOSim
  , runIOSimGen
    -- ** Explore races using /IOSimPOR/
  , monadicIOSimPOR_
  , monadicIOSimPOR
  , runIOSimPORGen
    -- $iosimpor
  , exploreSimTrace
  , controlSimTrace
  , ScheduleMod (..)
  , ScheduleControl (..)
    -- *** Exploration options
  , ExplorationSpec
  , ExplorationOptions (..)
  , stdExplorationOptions
  , withScheduleBound
  , withBranching
  , withStepTimelimit
  , withReplay
    -- * Lift ST computations
  , liftST
    -- * Simulation time
  , setCurrentTime
  , unshareClock
    -- * Simulation trace
  , type SimTrace
  , Time (..)
  , Trace (Cons, Nil, SimTrace, SimPORTrace, TraceDeadlock, TraceLoop, TraceMainReturn, TraceMainException, TraceRacesFound, TraceInternalError)
  , traceEvents
  , traceResult
  , SimResult (..)
  , SimEvent (..)
  , SimEventType (..)
  , ThreadLabel
  , IOSimThreadId (..)
  , Labelled (..)
  , Unique
    -- ** Dynamic Tracing
  , traceM
  , traceSTM
    -- ** Pretty printers
  , ppTrace
  , ppTrace_
  , ppEvents
  , ppSimEvent
  , ppDebug
    -- ** Selectors
    -- *** list selectors
  , selectTraceEvents
  , selectTraceEvents'
  , selectTraceEventsDynamic
  , selectTraceEventsDynamicWithTime
  , selectTraceEventsDynamic'
  , selectTraceEventsDynamicWithTime'
  , selectTraceEventsSay
  , selectTraceEventsSayWithTime
  , selectTraceEventsSay'
  , selectTraceEventsSayWithTime'
  , selectTraceRaces
    -- *** trace selectors
  , traceSelectTraceEvents
  , traceSelectTraceEventsDynamic
  , traceSelectTraceEventsSay
    -- ** IO printer
  , printTraceEventsSay
    -- * Eventlog
  , EventlogEvent (..)
  , EventlogMarker (..)
    -- * Low-level API
  , Timeout
  , newTimeout
  , readTimeout
  , cancelTimeout
  , awaitTimeout
  ) where

import Prelude

import Data.Bifoldable
import Data.Dynamic (fromDynamic)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Data.List.Trace (Trace (..))

import Control.Exception (SomeAsyncException (..), throw)

import Control.Monad.ST.Lazy

import Control.Monad.Class.MonadThrow as MonadThrow

import Control.Monad.IOSim.Internal (runSimTraceST)
import Control.Monad.IOSim.Types
import Control.Monad.IOSimPOR.Internal qualified as IOSimPOR (controlSimTraceST)
import Control.Monad.IOSimPOR.QuickCheckUtils

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import Test.QuickCheck.Monadic (PropertyM, monadic')

import System.IO.Unsafe

import Data.Functor (void)
import Data.IORef
import Debug.Trace qualified as Debug


-- | Select events according to the predicate function.  It throws an error if
-- the simulation ends with 'Failure'.
--
selectTraceEvents
    :: (Time -> SimEventType -> Maybe b)
    -> SimTrace a
    -> [b]
selectTraceEvents fn =
      bifoldr ( \ v _
               -> case v of
                    MainException _ _ e _     -> throw (FailureException e)
                    Deadlock      _   threads -> throw (FailureDeadlock threads)
                    MainReturn    _ _ _ _     -> []
                    Loop                      -> error "Impossible: selectTraceEvents _ TraceLoop{}"
                    InternalError msg         -> throw (FailureInternal msg)
              )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

-- | Like 'selectTraceEvents', but it returns even if the simulation trace ends
-- with 'Failure'.  It also works with any return type, not only `SimResult`
-- like `selectTraceEvents` does.
--
selectTraceEvents'
    :: (Time ->  SimEventType -> Maybe b)
    -> Trace a SimEvent
    -> [b]
selectTraceEvents' fn =
      bifoldr ( \ _ _   -> []  )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

selectTraceRaces :: SimTrace a -> [ScheduleControl]
selectTraceRaces = go
  where
    go (SimTrace _ _ _ _ trace)      = go trace
    go (SimPORTrace _ _ _ _ _ trace) = go trace
    go (TraceRacesFound races trace) =
      races ++ go trace
    go _                             = []

-- Extracting races from a trace.  There is a subtlety in doing so: we
-- must return a defined list of races even in the case where the
-- trace is infinite, and there are no races occurring in it! For
-- example, if the system falls into a deterministic infinite loop,
-- then there will be no races to find.

-- In reality we only want to extract races from *the part of the
-- trace used in a test*. We can only observe that by tracking lazy
-- evaluation: only races that were found in the evaluated prefix of
-- an infinite trace should contribute to the "races found". Hence we
-- return a function that returns the races found "so far". This is
-- unsafe, of course, since that function may return different results
-- at different times.

detachTraceRaces :: forall a. Int -> SimTrace a -> (() -> [ScheduleControl], SimTrace a)
detachTraceRaces debugLevel trace = unsafePerformIO $ do
  races <- newIORef []
  let readRaces :: () -> [ScheduleControl]
      readRaces () = concat . reverse . unsafePerformIO $ readIORef races

      saveRaces :: [ScheduleControl] -> x -> x
      saveRaces rs t = unsafePerformIO $ modifyIORef races (rs:)
                                      >> return t

      go :: SimTrace a -> SimTrace a
      go (SimTrace a b c d trace)      = traceDebugLog debugLevel (Left (SimEvent a b c d))
                                       $ SimTrace a b c d $ go trace
      go (SimPORTrace a b c d e trace) = traceDebugLog debugLevel (Left (SimPOREvent a b c d e))
                                       $ SimPORTrace a b c d e $ go trace
      go (TraceRacesFound rs trace)    = traceDebugLog debugLevel (Left (SimRacesFound rs))
                                       $ saveRaces rs $ go trace
      go (Cons a as)                   = traceDebugLog debugLevel (Left a)
                                       $ Cons a (go as)
      go (Nil a)                       = traceDebugLog debugLevel (Right a)
                                       $ Nil a

  return (readRaces, go trace)

-- | Select all the traced values matching the expected type.  It relies on the
-- sim's dynamic trace facility.
--
-- For convenience, it throws exceptions for abnormal sim termination.
--
selectTraceEventsDynamic :: forall a b. Typeable b => SimTrace a -> [b]
selectTraceEventsDynamic = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing

-- | Like 'selectTraceEventsDynamic' but it also captures time of the trace
-- event.
--
selectTraceEventsDynamicWithTime :: forall a b. Typeable b => SimTrace a -> [(Time, b)]
selectTraceEventsDynamicWithTime = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, b)
    fn t (EventLog dyn) = (t,) <$> fromDynamic dyn
    fn _ _              = Nothing

-- | Like 'selectTraceEventsDynamic' but it returns even if the simulation
-- trace ends with 'Failure'.  It also works with any return type, not only
-- `SimResult` like `selectTraceEventsDynamic` does.
--
selectTraceEventsDynamic' :: forall a b. Typeable b => Trace a SimEvent -> [b]
selectTraceEventsDynamic' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing

-- | Like `selectTraceEventsDynamicWithTime'` but it also captures time of the
-- trace event.  It also works with any return type, not only `SimResult` like
-- `selectTraceEventsDynamicWithTime` does.
--
selectTraceEventsDynamicWithTime' :: forall a b. Typeable b => Trace a SimEvent -> [(Time, b)]
selectTraceEventsDynamicWithTime' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, b)
    fn t (EventLog dyn) = (t,) <$> fromDynamic dyn
    fn _ _              = Nothing

-- | Get a trace of 'EventSay'.
--
-- For convenience, it throws exceptions for abnormal sim termination.
--
selectTraceEventsSay :: SimTrace a -> [String]
selectTraceEventsSay = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Like 'selectTraceEventsSay' but it also captures time of the trace event.
--
selectTraceEventsSayWithTime :: SimTrace a -> [(Time, String)]
selectTraceEventsSayWithTime = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, String)
    fn t (EventSay s) = Just (t, s)
    fn _ _            = Nothing

-- | Like 'selectTraceEventsSay' but it returns even if the simulation trace
-- ends with 'Failure'.  It also works with any return type, not only `SimResult`
-- like `selectTraceEventsSay` does.
--
selectTraceEventsSay' :: Trace a SimEvent -> [String]
selectTraceEventsSay' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Like `selectTraceEventsSay'` but it also captures time of the trace event.
--
selectTraceEventsSayWithTime' :: Trace a SimEvent -> [(Time, String)]
selectTraceEventsSayWithTime' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, String)
    fn t (EventSay s) = Just (t, s)
    fn _ _            = Nothing

-- | Print all 'EventSay' to the console.
--
-- For convenience, it throws exceptions for abnormal sim termination.
--
printTraceEventsSay :: SimTrace a -> IO ()
printTraceEventsSay = mapM_ print . selectTraceEventsSay


-- | The most general select function.  It is a /total function/.
--
traceSelectTraceEvents
    :: (Time -> SimEventType -> Maybe b)
    -> Trace a SimEvent
    -> Trace a b
traceSelectTraceEvents fn = bifoldr ( \ v _acc -> Nil v )
                                    ( \ eventCtx acc
                                     -> case eventCtx of
                                          SimRacesFound _ -> acc
                                          SimEvent{} ->
                                            case fn (seTime eventCtx) (seType eventCtx) of
                                              Nothing -> acc
                                              Just b  -> Cons b acc
                                          SimPOREvent{} ->
                                            case fn (seTime eventCtx) (seType eventCtx) of
                                              Nothing -> acc
                                              Just b  -> Cons b acc
                                    )
                                    undefined -- it is ignored

-- | Select dynamic events.  It is a /total function/.
--
traceSelectTraceEventsDynamic :: forall a b. Typeable b
                              => Trace a SimEvent
                              -> Trace a b
traceSelectTraceEventsDynamic = traceSelectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing


-- | Select say events.  It is a /total function/.
--
traceSelectTraceEventsSay :: forall a. Trace a SimEvent -> Trace a String
traceSelectTraceEventsSay = traceSelectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Simulation terminated a failure.
--
data Failure =
       -- | The main thread terminated with an exception.
       FailureException SomeException

       -- | The threads all deadlocked.
     | FailureDeadlock ![Labelled IOSimThreadId]

       -- | The main thread terminated normally but other threads were still
       -- alive, and strict shutdown checking was requested.
       -- See 'runSimStrictShutdown'.
     | FailureSloppyShutdown [Labelled IOSimThreadId]

       -- | An exception was thrown while evaluation the trace.
       -- This could be an internal assertion failure of `io-sim` or an
       -- unhandled exception in the simulation.
     | FailureEvaluation SomeException

       -- | An internal failure of the simulator.
       --
       -- Please open an issue at
       -- <https://github.com/intersectmbo/io-sim/issues>.
     | FailureInternal String
  deriving Show

instance Exception Failure where
    displayException (FailureException err) = displayException err
    displayException (FailureDeadlock threads) =
      concat [ "<<io-sim deadlock: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]
    displayException (FailureSloppyShutdown threads) =
      concat [ "<<io-sim sloppy shutdown: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]
    displayException (FailureEvaluation err) =
      concat [ "<<evaluation error: "
             , displayException  err
             , ">>"
             ]
    displayException (FailureInternal msg) =
      concat [ "<<internal failure: "
             , msg
             , ">>\n"
             , "please report the issue at\n"
             , "https://github.com/intersectmbo/io-sim/issues"
             ]


-- | 'IOSim' is a pure monad.
--
runSim :: forall a. (forall s. IOSim s a) -> Either Failure a
runSim mainAction = traceResult False (runSimTrace mainAction)

-- | For quick experiments and tests it is often appropriate and convenient to
-- simply throw failures as exceptions.
--
runSimOrThrow :: forall a. (forall s. IOSim s a) -> a
runSimOrThrow mainAction =
    case runSim mainAction of
      Left  e -> throw e
      Right x -> x

-- | Like 'runSim' but fail when the main thread terminates if there are other
-- threads still running or blocked. If one is trying to follow a strict thread
-- clean-up policy then this helps testing for that.
--
runSimStrictShutdown :: forall a. (forall s. IOSim s a) -> Either Failure a
runSimStrictShutdown mainAction = traceResult True (runSimTrace mainAction)

-- | Fold through the trace and return either 'Failure' or a simulation
-- result, i.e. the return value of the main thread.
--
traceResult :: Bool
            -- ^ if True the simulation will fail if there are any threads which
            -- didn't terminated when the main thread terminated.
            -> SimTrace a
            -- ^ simulation trace
            -> Either Failure a
traceResult strict = unsafePerformIO . eval
  where
    eval :: SimTrace a -> IO (Either Failure a)
    eval a = do
      -- NOTE: It's fine to let asynchronous exceptions pass through. The only
      -- way simulation could raise them is by using `throw` in pure code, while
      -- `throwIO` in the simulation will be captured as `FailureException`.  So
      -- we can safely assume asynchronous exceptions are coming from the
      -- environment running the simulation, e.g. `QuickCheck`, as in the case
      -- of `within` or `discardAfter` operators.
      r <- tryJust (\e -> case fromException @SomeAsyncException e of
                            Just _  -> Nothing
                            Nothing -> Just e)
                   (evaluate a)
      case r of
        Left e  -> return (Left (FailureEvaluation e))
        Right _ -> go a

    go :: SimTrace a -> IO (Either Failure a)
    go (SimTrace _ _ _ _ t)             = eval t
    go (SimPORTrace _ _ _ _ _ t)        = eval t
    go (TraceRacesFound _ t)            = eval t
    go (TraceMainReturn _ _ _ tids@(_:_))
                               | strict = pure $ Left (FailureSloppyShutdown tids)
    go (TraceMainReturn _ _ x _)        = pure $ Right x
    go (TraceMainException _ _ e _)     = pure $ Left (FailureException e)
    go (TraceDeadlock   _   threads)    = pure $ Left (FailureDeadlock threads)
    go TraceLoop{}                      = error "Impossible: traceResult TraceLoop{}"
    go (TraceInternalError msg)         = pure $ Left (FailureInternal msg)

-- | Turn 'SimTrace' into a list of timestamped events.
--
traceEvents :: SimTrace a -> [(Time, IOSimThreadId, Maybe ThreadLabel, SimEventType)]
traceEvents (SimTrace time tid tlbl event t)      = (time, tid, tlbl, event)
                                                  : traceEvents t
traceEvents (SimPORTrace time tid _ tlbl event t) = (time, tid, tlbl, event)
                                                  : traceEvents t
traceEvents _                                     = []


-- | Pretty print a timestamped event.
--
ppEvents :: [(Time, IOSimThreadId, Maybe ThreadLabel, SimEventType)]
         -> String
ppEvents events =
    intercalate "\n"
      [ ppSimEvent timeWidth tidWidth width
                   SimEvent {seTime, seThreadId, seThreadLabel, seType }
      | (seTime, seThreadId, seThreadLabel, seType) <- events
      ]
  where
    timeWidth = maximum
                [ length (show t)
                | (t, _, _, _) <- events
                ]
    tidWidth  = maximum
                [ length (show tid)
                | (_, tid, _, _) <- events
                ]
    width     = maximum
                [ maybe 0 length threadLabel
                | (_, _, threadLabel, _) <- events
                ]


-- | See 'runSimTraceST' below.
--
runSimTrace :: forall a. (forall s. IOSim s a) -> SimTrace a
runSimTrace mainAction = runST (runSimTraceST mainAction)

--
-- IOSimPOR
--
--
-- $iosimpor
--
-- /IOSimPOR/ is a different interpreter of 'IOSim' which has the ability to
-- discover race conditions and replay the simulation using a schedule which
-- reverts them.  For extended documentation how to use it see
-- [here](https://github.com/intersectmbo/io-sim/blob/main/io-sim/how-to-use-IOSimPOR.md).
--
-- /IOSimPOR/ only discovers races between events which happen in the same time
-- slot.  In /IOSim/ and /IOSimPOR/ time only moves explicitly through timer
-- events, e.g. things like `Control.Monad.Class.MonadTimer.SI.threadDelay`,
-- `Control.Monad.Class.MonadTimer.SI.registerDelay` or the
-- `Control.Monad.Class.MonadTimer.MonadTimeout.NonStandard` API.  The usual
-- QuickCheck techniques can help explore different schedules of
-- threads too.

-- | Execute a simulation, discover & revert races.  Note that this will execute
-- the simulation multiple times with different schedules, and thus it's much
-- more costly than a simple `runSimTrace` (also the simulation environments has
-- much more state to track and hence it is slower).
--
-- On property failure it will show the failing schedule (`ScheduleControl`)
-- which can be passed to `controlSimTrace` to reproduce the failure without
-- discovering the schedule.
--
exploreSimTrace
  :: forall a test. Testable test
  => (ExplorationOptions -> ExplorationOptions)
  -- ^ modify default exploration options
  -> (forall s. IOSim s a)
  -- ^ a simulation to run
  -> (Maybe (SimTrace a) -> SimTrace a -> test)
  -- ^ a callback which receives the previous trace (e.g. before reverting
  -- a race condition) and current trace
  -> Property
exploreSimTrace optsf mainAction k =
  case explorationReplay opts of
    Nothing ->
      explore (explorationScheduleBound opts) (explorationBranching opts) ControlDefault Nothing .&&.
      let size = cacheSize() in size `seq`
      tabulate "Modified schedules explored" [bucket size] True
    Just control ->
      replaySimTrace opts mainAction control (k Nothing)
  where
    opts = optsf stdExplorationOptions

    explore :: Int -- schedule bound
            -> Int -- branching factor
            -> ScheduleControl -> Maybe (SimTrace a) -> Property
    explore n m control passingTrace =

      -- ALERT!!! Impure code: readRaces must be called *after* we have
      -- finished with trace.
      let (readRaces, trace0) = detachTraceRaces (explorationDebugLevel opts)
                              $ controlSimTrace
                                  (explorationStepTimelimit opts) control mainAction
          (sleeper,trace) = compareTraces passingTrace trace0
      in ( counterexample ("Schedule control: " ++ show control)
         $ counterexample
            (case sleeper of
              Nothing -> "No thread delayed"
              Just ((t,tid,lab),racing) ->
                showThread (tid,lab) ++
                " delayed at time "++
                show t ++
                "\n  until after:\n" ++
                unlines (map (("    "++).showThread) $ Set.toList racing)
             )
         $ k passingTrace trace
         )
      .&&| let limit     = (n+m-1) `div` m
               -- To ensure the set of schedules explored is deterministic, we
               -- filter out cached ones *after* selecting the children of this
               -- node.
               races     = filter (not . cached) . take limit $ readRaces ()
               branching = length races
           in -- tabulate "Races explored" (map show races) $
              tabulate "Branching factor" [bucket branching] $
              tabulate "Race reversals per schedule" [bucket (raceReversals control)] $
              conjoinPar
                [ --Debug.trace "New schedule:" $
                  --Debug.trace ("  "++show r) $
                  --counterexample ("Schedule control: " ++ show r) $
                  explore n' ((m-1) `max` 1) r (Just trace0)
                | (r,n') <- zip races (divide (n-branching) branching) ]

    bucket :: Int -> String
    bucket n | n<10      = show n
             | otherwise = buck n 1
    buck n t | n<10      = show (n*t) ++ "-" ++ show ((n+1)*t-1)
             | otherwise = buck (n `div` 10) (t*10)

    -- divide n into k factors which sums up to n
    divide :: Int -> Int -> [Int]
    divide n k =
      [ n `div` k + if i < n `mod` k then 1 else 0
      | i <- [0..k-1] ]

    showThread :: (IOSimThreadId,Maybe ThreadLabel) -> String
    showThread (tid,lab) =
      ppIOSimThreadId tid ++ (case lab of Nothing -> ""
                                          Just l  -> " ("++l++")")

    -- cache of explored schedules
    cache :: IORef (Set ScheduleControl)
    cache = unsafePerformIO cacheIO

    -- insert a schedule into the cache
    cached :: ScheduleControl -> Bool
    cached = unsafePerformIO . cachedIO

    -- compute cache size; it's a function to make sure that `GHC` does not
    -- inline it (and share the same thunk).
    cacheSize :: () -> Int
    cacheSize = unsafePerformIO . cacheSizeIO

    --
    -- Caching in IO monad
    --

    -- It is possible for the same control to be generated several times.
    -- To avoid exploring them twice, we keep a cache of explored schedules.
    cacheIO :: IO (IORef (Set ScheduleControl))
    cacheIO = newIORef $
              -- we use opts here just to be sure the reference cannot be
              -- lifted out of exploreSimTrace
              if explorationScheduleBound opts >=0
                then Set.empty
                else error "exploreSimTrace: negative schedule bound"

    cachedIO :: ScheduleControl -> IO Bool
    cachedIO m = atomicModifyIORef' cache $ \set ->
      (Set.insert m set, Set.member m set)

    cacheSizeIO :: () -> IO Int
    cacheSizeIO () = Set.size <$> readIORef cache


-- |  Trace `SimTrace` to `stderr`.
--
-- An internal function.
--
traceDebugLog :: Int -> Either SimEvent (SimResult a) -> SimTrace a -> SimTrace a
traceDebugLog logLevel _event trace | logLevel <= 0 = trace
-- Discard races if log level is 1
traceDebugLog 1 (Left SimPOREvent { seType = EventRaces {} }) trace = trace
traceDebugLog 1 (Left event) trace  = Debug.trace (ppSimEvent 0 0 0 event) trace
traceDebugLog 1 (Right event) trace = Debug.trace (ppSimResult 0 0 0 (void event)) trace
-- Otherwise, show races
traceDebugLog _ (Left event) trace  = Debug.trace (ppSimEvent 0 0 0 event) trace
traceDebugLog _ (Right event) trace = Debug.trace (ppSimResult 0 0 0 (void event)) trace

replaySimTrace :: forall a test. (Testable test)
               => ExplorationOptions
               -- ^ race exploration options
               -> (forall s. IOSim s a)
               -> ScheduleControl
               -- ^ a schedule control to reproduce
               -> (SimTrace a -> test)
               -- ^ a callback which receives the simulation trace. The trace
               -- will not contain any race events
               -> Property
replaySimTrace opts mainAction control k =
  let (_,trace) = detachTraceRaces (explorationDebugLevel opts) $
                  controlSimTrace (explorationStepTimelimit opts) control mainAction
  in property (k trace)

-- | Run a simulation using a given schedule.  This is useful to reproduce
-- failing cases without exploring the races.
--
controlSimTrace :: forall a.
                   Maybe Int
                -- ^ limit on the computation time allowed per scheduling step, for
                -- catching infinite loops etc.
                -> ScheduleControl
                -- ^ a schedule to replay
                --
                -- /note/: must be either `ControlDefault` or `ControlAwait`.
                -> (forall s. IOSim s a)
                -- ^ a simulation to run
                -> SimTrace a
controlSimTrace limit control main =
    runST (IOSimPOR.controlSimTraceST limit control main)

--
-- Utils
--

raceReversals :: ScheduleControl -> Int
raceReversals ControlDefault      = 0
raceReversals (ControlAwait mods) = length mods
raceReversals ControlFollow{}     = error "Impossible: raceReversals ControlFollow{}"

-- `compareTracesST` is given (maybe) a passing trace and a failing trace,
-- and identifies the point at which they diverge, where it inserts a
-- "sleep" event for the thread that is delayed in the failing case,
-- and a "wake" event before its next action. It also returns the
-- identity and time of the sleeping thread. Since we expect the trace
-- to be consumed lazily (and perhaps only partially), and since the
-- sleeping thread is not of interest unless the trace is consumed
-- this far, then we collect its identity only if it is reached using
-- `unsafePerformIO`.

compareTraces :: Maybe (SimTrace a1)
              -> SimTrace a2
              -> (Maybe ((Time, IOSimThreadId, Maybe ThreadLabel),
                         Set.Set (IOSimThreadId, Maybe ThreadLabel)),
                  SimTrace a2)
compareTraces Nothing trace = (Nothing, trace)
compareTraces (Just passing) trace = unsafePerformIO $ do
  sleeper <- newIORef Nothing
  return (unsafePerformIO $ readIORef sleeper,
          go sleeper passing trace)
  where go sleeper (SimPORTrace tpass tidpass _ _ _ pass')
                   (SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | (tpass,tidpass) == (tfail,tidfail) =
              SimPORTrace tfail tidfail tstepfail tlfail evfail
                $ go sleeper pass' fail'
        go sleeper (SimPORTrace tpass tidpass tsteppass tlpass _ _) fail =
          unsafePerformIO $ do
            writeIORef sleeper $ Just ((tpass, tidpass, tlpass),Set.empty)
            return $ SimPORTrace tpass tidpass tsteppass tlpass EventThreadSleep
                   $ wakeup sleeper tidpass fail
        go _ SimTrace {} _ = error "compareTraces: invariant violation"
        go _ _ SimTrace {} = error "compareTraces: invariant violation"
        go _ _ fail = fail

        wakeup sleeper tidpass
               fail@(SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | tidpass == tidfail =
              SimPORTrace tfail tidfail tstepfail tlfail EventThreadWake fail
          | otherwise = unsafePerformIO $ do
              Just (slp,racing) <- readIORef sleeper
              writeIORef sleeper $ Just (slp,Set.insert (tidfail,tlfail) racing)
              return $ SimPORTrace tfail tidfail tstepfail tlfail evfail
                     $ wakeup sleeper tidpass fail'
        wakeup _ _ SimTrace {} = error "compareTraces: invariant violation"
        wakeup _ _ fail = fail

--
-- QuickCheck monadic combinators
--



-- | Like <monadicST https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:monadicST>.
--
-- Note: it calls `traceResult` in non-strict mode, e.g. leaked threads do not
-- cause failures.
--
-- @since 1.4.1.0
--
monadicIOSim_ :: Testable a
              => (forall s. PropertyM (IOSim s) a)
              -> Property
monadicIOSim_ sim =
    monadicIOSim
      (\e -> case traceResult False e of
          Left e  -> counterexample (show e) False
          Right p -> p)
      id
      sim

-- | A more general version of `monadicIOSim_`, which:
--
-- * allows to run in monad stacks build on top of `IOSim`;
-- * gives more control how to attach debugging information to failed
--   tests.
--
-- Note, to use this combinator your monad needs to be defined as:
--
-- > newtype M s a = M s { runM :: ReaderT State (IOSim s) a }
--
-- It's important that `M s` is a monad.  For such a monad one you'll need provide
-- a natural transformation:
-- @
--   -- the state could also be created as an `IOSim` computation.
--   nat :: forall s a. State -> M s a -> 'IOSim' s a
--   nat state m = runStateT (runM m) state
-- @
--
-- @since 1.4.1.0
--
monadicIOSim :: (Testable a, forall s. Monad (m s))
             => (SimTrace Property -> Property)
             -- ^ Allows to trace `SimTrace` in counterexamples.  The simplest
             -- use case is to pass:
             --
             -- > either (\e -> counterexample (show e) False) id . traceResult False
             --
             -- as `monadicIOSim_` does.
             --
             -> (forall s a. m s a -> IOSim s a)
             -- ^ natural transformation from `m` to @IOSim` s@
             -> (forall s. PropertyM (m s) a)
             -> Property
monadicIOSim f tr sim = property (runIOSimGen f (tr <$> monadic' sim))

-- | Like <runSTGen
-- https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:runSTGen>.
--
-- @since 1.4.1.0
--
runIOSimGen :: (SimTrace a -> Property)
            -> (forall s. Gen (IOSim s a))
            -> Gen Property
runIOSimGen f sim = do
    Capture eval <- capture
    let trace = runSimTrace (eval sim)
    return (f trace)

-- | Like <runSTGen
-- https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:runSTGen>,
-- but evaluates generated simulations using 'exploreSimTrace'.
--
-- The callback receives the previously passing trace (if any) and the current
-- trace being checked.
--
-- @since 1.10.0.0
--
runIOSimPORGen :: Testable test
               => (ExplorationOptions -> ExplorationOptions)
               -> (Maybe (SimTrace a) -> SimTrace a -> test)
               -> (forall s. Gen (IOSim s a))
               -> Gen Property
runIOSimPORGen optsf k sim = do
  Capture eval <- capture
  pure $ exploreSimTrace optsf (eval sim) k

-- | A /IOSimPOR/ variant of 'monadicIOSim_', which:
--
-- * explores alternative schedules using 'exploreSimTrace';
-- * allows to run in monad stacks build on top of `IOSim`;
-- * gives more control how to attach debugging information to failed
--   tests.
--
-- Note, to use this combinator your monad needs to be defined as:
--
-- > newtype M s a = M { runM :: ReaderT State (IOSim s) a }
--
-- It's important that `M s` is a monad.  For such a monad you'll need to
-- provide a natural transformation:
-- @
--   -- the state could also be created as an `IOSim` computation.
--   nat :: forall s a. State -> M s a -> 'IOSim' s a
--   nat state m = runStateT (runM m) state
-- @
--
-- @since 1.10.0.0
--
monadicIOSimPOR :: (Testable a, forall s. Monad (m s))
                => (ExplorationOptions -> ExplorationOptions)
                -> (Maybe (SimTrace Property) -> SimTrace Property -> Property)
                -> (forall s a. m s a -> IOSim s a)
                -> (forall s. PropertyM (m s) a)
                -> Property
monadicIOSimPOR optsf f tr sim =
  property (runIOSimPORGen optsf f (tr <$> monadic' sim))

-- | Like <monadicST
-- https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:monadicST>,
-- but using /IOSimPOR/ schedule exploration.
--
-- Note: it calls `traceResult` in non-strict mode, e.g. leaked threads do not
-- cause failures.
--
-- > testProperty "example" $
-- >   monadicIOSimPOR_ $ do
-- >     x <- run (pure (1 :: Int))
-- >     assert (x == 1)
--
-- @since 1.10.0.0
--
monadicIOSimPOR_ :: Testable a
                 => (forall s. PropertyM (IOSim s) a)
                 -> Property
monadicIOSimPOR_ sim =
   monadicIOSimPOR
      id
      (\_ tr -> case traceResult False tr of
          Left e  -> counterexample (show e) False
          Right p -> p)
      id
      sim
