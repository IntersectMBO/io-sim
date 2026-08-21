{-# LANGUAGE CPP             #-}
{-# LANGUAGE DerivingVia     #-}
-- only used to construct records if its clear to do so
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
-- We use partial functions from `Data.List`.
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Control.Monad.IOSimPOR.Internal
  ( IOSim (..)
  , runIOSim
  , runSimTraceST
  , traceM
  , traceSTM
  , STM
  , STMSim
  , setCurrentTime
  , unshareClock
  , TimeoutException (..)
  , EventlogEvent (..)
  , EventlogMarker (..)
  , IOSimThreadId
  , ThreadLabel
  , Labelled (..)
  , SimTrace
  , Trace.Trace (SimPORTrace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , SimEvent (..)
  , SimResult (..)
  , SimEventType (..)
  , liftST
  , execReadTVar
  , controlSimTraceST
  , ScheduleControl (..)
  , ScheduleMod (..)
  ) where

import Prelude hiding (read)

import Data.Dynamic
import Data.Foldable (foldlM, traverse_)
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as PSQ
import Data.IntPSQ (IntPSQ)
import Data.IntPSQ qualified as IPSQ
import Data.List qualified as List
import Data.List.Trace qualified as Trace
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime (..), fromGregorian)

import Control.Exception (NonTermination (..), SomeAsyncException, assert,
           throw)
import Control.Monad (join, when)
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe (unsafeIOToST, unsafeInterleaveST)
import Data.STRef.Lazy

import Control.Concurrent.Class.MonadSTM.TMVar
import Control.Concurrent.Class.MonadSTM.TVar hiding (TVar)
import Control.Monad.Class.MonadFork (killThread, myThreadId, throwTo)
import Control.Monad.Class.MonadSTM hiding (STM)
import Control.Monad.Class.MonadSTM.Internal (TMVarDefault (TMVar))
import Control.Monad.Class.MonadThrow as MonadThrow
import Control.Monad.Class.MonadTime (NominalDiffTime)
import Control.Monad.Class.MonadTime qualified as Time
import Control.Monad.Class.MonadTime.SI qualified as SI
import Control.Monad.Class.MonadTimer.SI (TimeoutState (..))

import Control.Monad.IOSim.InternalTypes
import Control.Monad.IOSim.Types hiding (SimEvent (SimEvent), Time (..),
           Trace (SimTrace))
import Control.Monad.IOSim.Types (SimEvent)
import Control.Monad.IOSimPOR.Timeout (unsafeTimeout)
import Control.Monad.IOSimPOR.Types
import Data.Coerce (Coercible, coerce)
import Data.Hashable

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !IOSimThreadId,
    threadControl :: !(ThreadControl s a),
    threadStatus  :: !ThreadStatus,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled IOSimThreadId, VectorClock)],
    threadClockId :: !ClockId,
    threadLabel   :: Maybe ThreadLabel,
    threadNextTId :: !Int,
    threadStep    :: !Int,
    threadVClock  :: VectorClock,
    threadEffect  :: Effect,  -- in the current step
    threadRacy    :: !Bool
  }
  deriving Show

isThreadBlocked :: Thread s a -> Bool
isThreadBlocked t = case threadStatus t of
    ThreadBlocked {} -> True
    _                -> False

isThreadDone :: Thread s a -> Bool
isThreadDone t = case threadStatus t of
    ThreadDone -> True
    _          -> False

threadStepId :: Thread s a -> (IOSimThreadId, Int)
threadStepId Thread{ threadId, threadStep } = (threadId, threadStep)

isRacyThreadId :: IOSimThreadId -> Bool
isRacyThreadId (RacyThreadId _) = True
isRacyThreadId _                = True

isNotRacyThreadId :: IOSimThreadId -> Bool
isNotRacyThreadId (ThreadId _) = True
isNotRacyThreadId _            = False

bottomVClock :: VectorClock
bottomVClock = VectorClock Map.empty

insertVClock :: IOSimThreadId -> Int -> VectorClock -> VectorClock
insertVClock tid !step (VectorClock m) = VectorClock (Map.insert tid step m)

leastUpperBoundVClock :: VectorClock -> VectorClock -> VectorClock
leastUpperBoundVClock (VectorClock m) (VectorClock m') =
    VectorClock (Map.unionWith max m m')

-- hbfVClock :: VectorClock -> VectorClock -> Bool
-- hbfVClock (VectorClock m) (VectorClock m') = Map.isSubmapOfBy (<=) m m'

happensBeforeStep :: Step -- ^ an earlier step
                  -> Step -- ^ a later step
                  -> Bool
happensBeforeStep step step' =
       Just (stepStep step)
    <= Map.lookup (stepThreadId step)
                  (getVectorClock $ stepVClock step')

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = Labelled tvarId <$> readSTRef tvarLabel

labelledThreads :: Map IOSimThreadId (Thread s a) -> [Labelled IOSimThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not retain the
    -- original thread map we need to evaluate the spine of the list.
    -- TODO: https://github.com/haskell/containers/issues/749
    Map.foldr'
      (\Thread { threadId, threadLabel } !acc -> Labelled threadId threadLabel : acc)
      [] threadMap


-- | Timers mutable variables.  First one supports 'newTimeout' api, the second
-- one 'Control.Monad.Class.MonadTimer.SI.registerDelay', the third one
-- 'Control.Monad.Class.MonadTimer.SI.threadDelay'.
--
data TimerCompletionInfo s =
       Timer !(TVar s TimeoutState)
     -- ^ `newTimeout` timer.
     | TimerRegisterDelay !(TVar s Bool)
     -- ^ `registerDelay` timer.
     | TimerThreadDelay !IOSimThreadId !TimeoutId
     -- ^ `threadDelay` timer run by `IOSimThreadId` which was assigned the given
     -- `TimeoutId` (only used to report in a trace).
     | TimerTimeout !IOSimThreadId !TimeoutId !(TMVar (IOSim s) IOSimThreadId)
     -- ^ `timeout` timer run by `IOSimThreadId` which was assigned the given
     -- `TimeoutId` (only used to report in a trace).

instance Hashable a => Hashable (Down a)

type RunQueue   = HashPSQ (Down IOSimThreadId) (Down IOSimThreadId) ()
type Timeouts s = IntPSQ SI.Time (TimerCompletionInfo s)

-- | Internal state.
--
data SimState s a = SimState {
       runqueue         :: !RunQueue,
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads          :: !(Map IOSimThreadId (Thread s a)),
       -- | current time
       curTime          :: !SI.Time,
       -- | ordered list of timers and timeouts
       timers           :: !(Timeouts s),
       -- | timeout locks in order to synchronize the timeout handler and the
       -- main thread
       clocks           :: !(Map ClockId UTCTime),
       nextVid          :: !VarId,     -- ^ next unused 'TVarId'
       nextTmid         :: !TimeoutId,  -- ^ next unused 'TimeoutId'
       nextUniq         :: !(Unique s), -- ^ next unused @'Unique' s@
       -- | previous steps (which we may race with).
       -- Note this is *lazy*, so that we don't compute races we will not reverse.
       races            :: Races,
       -- | control the schedule followed, and initial value
       control          :: !ScheduleControl,
       control0         :: !ScheduleControl,
       -- | limit on the computation time allowed per scheduling step, for
       -- catching infinite loops etc
       perStepTimeLimit :: Maybe Int

     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = PSQ.empty,
      threads  = Map.empty,
      curTime  = SI.Time 0,
      timers   = IPSQ.empty,
      clocks   = Map.singleton (ClockId []) epoch1970,
      nextVid  = 0,
      nextTmid = TimeoutId 0,
      nextUniq = MkUnique 0,
      races    = noRaces,
      control  = ControlDefault,
      control0 = ControlDefault,
      perStepTimeLimit = Nothing
    }
  where
    epoch1970 = UTCTime (fromGregorian 1970 1 1) 0

invariant :: Maybe (Thread s a) -> SimState s a -> x -> x

invariant (Just running) simstate@SimState{runqueue,threads,clocks} =
    assert (not (isThreadBlocked running))
  . assert (threadId running `Map.notMember` threads)
  . assert (not (Down (threadId running) `PSQ.member` runqueue))
  . assert (threadClockId running `Map.member` clocks)
  . invariant Nothing simstate

invariant Nothing SimState{runqueue,threads,clocks} =
    assert (PSQ.fold' (\(Down tid) _ _ a -> tid `Map.member` threads && a) True runqueue)
  . assert (and [ (isThreadBlocked t || isThreadDone t) == not (Down (threadId t) `PSQ.member` runqueue)
                | t <- Map.elems threads ])
  . assert (and [ threadClockId t `Map.member` clocks
                | t <- Map.elems threads ])

-- | Interpret the simulation monotonic time as a 'NominalDiffTime' since
-- the start.
timeSinceEpoch :: SI.Time -> NominalDiffTime
timeSinceEpoch (SI.Time t) = fromRational (toRational t)


-- | Insert thread into `runqueue`.
--
insertThread :: Thread s a -> RunQueue -> RunQueue
insertThread Thread { threadId } = PSQ.insert (Down threadId) (Down threadId) ()


-- | Schedule / run a thread.
--
schedule :: forall s a. Thread s a -> SimState s a -> ST s (SimTrace a)
schedule thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst,
           threadLabel   = tlbl,
           threadStep    = tstep,
           threadVClock  = vClock,
           threadEffect  = effect
         }
         simstate@SimState {
           runqueue,
           threads,
           timers,
           clocks,
           nextVid, nextTmid, nextUniq,
           curTime  = time,
           control,
           perStepTimeLimit
         }

  | controlTargets (tid,tstep) control =
      -- The next step is to be delayed according to the
      -- specified schedule. Switch to following the schedule.
      SimPORTrace time tid tstep tlbl (EventFollowControl control) <$>
      schedule thread simstate{ control = followControl control }

  | not $ controlFollows (tid,tstep) control =
      -- the control says this is not the next step to
      -- follow. We should be at the beginning of a step;
      -- we put the present thread to sleep and reschedule
      -- the correct thread.
      -- The assertion says that the only effect that may have
      -- happened in the start of a thread is us waking up.
      ( SimPORTrace time tid tstep tlbl (EventAwaitControl (tid,tstep) control)
      . SimPORTrace time tid tstep tlbl (EventDeschedule Sleep)
      ) <$> deschedule Sleep thread simstate

  | otherwise =
  invariant (Just thread) simstate $
  case control of
    ControlFollow (s:_) _
      -> fmap (SimPORTrace time tid tstep tlbl (EventPerformAction (tid,tstep)))
    _ -> id
  $
  -- The next line forces the evaluation of action, which should be unevaluated up to
  -- this point. This is where we actually *run* user code.
  case maybe Just unsafeTimeout perStepTimeLimit action of
    Nothing -> return TraceLoop
    Just _  -> case action of

      Return x -> case ctl of
        MainFrame ->
          -- the main thread is done, so we're done
          -- even if other threads are still running
          return $ SimPORTrace time tid tstep tlbl EventThreadFinished
                 $ traceFinalRacesFound simstate
                 $ TraceMainReturn time (Labelled tid tlbl) x
                                        ( labelledThreads
                                        . Map.filter (not . isThreadDone)
                                        $ threads
                                        )

        ForkFrame -> do
          -- this thread is done
          let thread' = thread
          !trace <- deschedule Terminated thread' simstate
          return $ SimPORTrace time tid tstep tlbl EventThreadFinished
                 $ SimPORTrace time tid tstep tlbl (EventDeschedule Terminated)
                 $ trace

        MaskFrame k maskst' ctl' -> do
          -- pop the control stack, restore thread-local state
          let thread' = thread { threadControl = ThreadControl (k x) ctl'
                               , threadMasking = maskst'
                               }
          -- but if we're now unmasked, check for any pending async exceptions
          !trace <- deschedule Interruptable thread' simstate
          return $ SimPORTrace time tid tstep tlbl (EventMask maskst')
                 $ SimPORTrace time tid tstep tlbl (EventDeschedule Interruptable)
                 $ trace

        CatchFrame _handler k ctl' -> do
          -- pop the control stack and continue
          let thread' = thread { threadControl = ThreadControl (k x) ctl' }
          schedule thread' simstate

        TimeoutFrame tmid lock k ctl' -> do
          -- It could happen that the timeout action finished at the same time
          -- as the timeout expired, this will be a race condition. That's why
          -- we have the locks to solve this.

          -- We cannot do `tryPutMVar` in the `treadAction`, because we need to
          -- know if the `lock` is empty right now when we still have the frame.
          v <- execTryPutTMVar lock undefined
          let -- Kill the assassin throwing thread then unmask exceptions and
              -- carry on the continuation
              threadAction :: IOSim s ()
              threadAction =
                if v then unsafeUnregisterTimeout tmid
                     else atomically (takeTMVar lock) >>= killThread

              thread' =
                thread { threadControl =
                          ThreadControl (case threadAction of
                                          IOSim k' -> k' (\() -> k (Just x)))
                                        ctl'
                       }
          schedule thread' simstate

        DelayFrame tmid k ctl' -> do
          let thread' = thread { threadControl = ThreadControl k ctl' }
              timers' = IPSQ.delete (coerce tmid) timers
          schedule thread' simstate { timers = timers' }

      Throw e -> case unwindControlStack e thread timers of
        -- Found a CatchFrame
        (Right thread0@Thread { threadMasking = maskst' }, timers'') -> do
          -- We found a suitable exception handler, continue with that
          -- We record a step, in case there is no exception handler on replay.
          let (thread', eff)  = stepThread thread0
              control'        = advanceControl (threadStepId thread0) control
              races'          = updateRaces thread0 simstate
          trace <- schedule thread' simstate{ races = races',
                                              control = control',
                                              timers = timers'' }
          return (SimPORTrace time tid tstep tlbl (EventThrow e) $
                  SimPORTrace time tid tstep tlbl (EventMask maskst') $
                  SimPORTrace time tid tstep tlbl (EventEffect vClock eff) $
                  SimPORTrace time tid tstep tlbl (EventRaces races')
                  trace)

        (Left isMain, timers'')
          -- We unwound and did not find any suitable exception handler, so we
          -- have an unhandled exception at the top level of the thread.
          | isMain -> do
            let thread' = thread { threadStatus = ThreadDone }
            -- An unhandled exception in the main thread terminates the program
            return (SimPORTrace time tid tstep tlbl (EventThrow e) $
                    SimPORTrace time tid tstep tlbl (EventThreadUnhandled e) $
                    traceFinalRacesFound simstate { threads = Map.insert tid thread' threads } $
                    TraceMainException time (Labelled tid tlbl) e (labelledThreads threads))

          | otherwise -> do
            -- An unhandled exception in any other thread terminates the thread
            let terminated = Terminated
            !trace <- deschedule terminated thread simstate { timers = timers'' }
            return $ SimPORTrace time tid tstep tlbl (EventThrow e)
                   $ SimPORTrace time tid tstep tlbl (EventThreadUnhandled e)
                   $ SimPORTrace time tid tstep tlbl (EventDeschedule terminated)
                   $ trace

      Catch action' handler k -> do
        -- push the failure and success continuations onto the control stack
        let thread' = thread { threadControl = ThreadControl action'
                                                 (CatchFrame handler k ctl)
                             }
        schedule thread' simstate

      Evaluate expr k -> do
        mbWHNF <- unsafeIOToST $ tryJust (\e -> case fromException @SomeAsyncException e of
                                            Nothing -> Just e
                                            Just {} -> Nothing)
                               $ evaluate expr
        case mbWHNF of
          Left e -> do
            -- schedule this thread to immediately raise the exception
            let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
            trace <- schedule thread' simstate
            return $ SimPORTrace time tid tstep tlbl (EventEvaluationError e)
                   $ trace
          Right whnf -> do
            -- continue with the resulting WHNF
            let thread' = thread { threadControl = ThreadControl (k whnf) ctl }
            trace <- schedule thread' simstate
            return $ SimPORTrace time tid tstep tlbl EventEvaluationSuccess
                   $ trace

      Say msg k -> do
        let thread' = thread { threadControl = ThreadControl k ctl }
        trace <- schedule thread' simstate
        return (SimPORTrace time tid tstep tlbl (EventSay msg) trace)

      Output x k -> do
        let thread' = thread { threadControl = ThreadControl k ctl }
        trace <- schedule thread' simstate
        return (SimPORTrace time tid tstep tlbl (EventLog x) trace)

      LiftST st k -> do
        x <- strictToLazyST st
        let thread' = thread { threadControl = ThreadControl (k x) ctl }
        schedule thread' simstate

      GetMonoTime k -> do
        let thread' = thread { threadControl = ThreadControl (k time) ctl }
        schedule thread' simstate

      GetWallTime k -> do
        let clockid  = threadClockId thread
            clockoff = clocks Map.! clockid
            walltime = timeSinceEpoch time `Time.addUTCTime` clockoff
            thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
        schedule thread' simstate

      SetWallTime walltime' k -> do
        let clockid   = threadClockId thread
            clockoff  = clocks Map.! clockid
            walltime  = timeSinceEpoch time `Time.addUTCTime` clockoff
            clockoff' = (walltime' `Time.diffUTCTime` walltime) `Time.addUTCTime` clockoff
            thread'   = thread { threadControl = ThreadControl k ctl }
            simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
        schedule thread' simstate'

      UnshareClock k -> do
        let clockid   = threadClockId thread
            clockoff  = clocks Map.! clockid
            clockid'  = let ThreadId i = tid in ClockId i -- reuse the thread id
            thread'   = thread { threadControl = ThreadControl k ctl
                               , threadClockId = clockid' }
            simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
        schedule thread' simstate'

      -- This case is guarded by checks in 'timeout' itself.
      StartTimeout d _ _ | d <= 0 ->
        error "schedule: StartTimeout: Impossible happened"

      StartTimeout d action' k -> do
        lock <- TMVar <$> execNewTVar (TMVarId nextVid) (Just $! "lock-" ++ show nextTmid) Nothing
        let expiry    = d `addTime` time
            timers'   = IPSQ.insert (coerce nextTmid) expiry (TimerTimeout tid nextTmid lock) timers
            thread'   = thread { threadControl =
                                   ThreadControl action'
                                                 (TimeoutFrame nextTmid lock k ctl)
                                }
        trace <- deschedule Yield thread' simstate { timers   = timers'
                                                    , nextTmid = succ nextTmid }
        return (SimPORTrace time tid tstep tlbl (EventTimeoutCreated nextTmid tid expiry) trace)

      UnregisterTimeout tmid k -> do
        let thread' = thread { threadControl = ThreadControl k ctl }
        schedule thread' simstate { timers = IPSQ.delete (coerce tmid) timers }

      RegisterDelay d k | d < 0 -> do
        tvar <- execNewTVar (TVarId nextVid)
                            (Just $! "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                            True
        modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
        let !expiry  = d `addTime` time
            !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
        trace <- schedule thread' simstate { nextVid = succ nextVid }
        return (SimPORTrace time tid tstep tlbl (EventRegisterDelayCreated nextTmid (TVarId nextVid) expiry) $
                SimPORTrace time tid tstep tlbl (EventRegisterDelayFired nextTmid) $
                trace)

      RegisterDelay d k -> do
        tvar <- execNewTVar (TVarId nextVid)
                            (Just $! "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                            False
        modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
        let !expiry  = d `addTime` time
            !timers' = IPSQ.insert (coerce nextTmid) expiry (TimerRegisterDelay tvar) timers
            !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
        trace <- schedule thread' simstate { timers   = timers'
                                           , nextVid  = succ nextVid
                                           , nextTmid = succ nextTmid }
        return (SimPORTrace time tid tstep tlbl
                  (EventRegisterDelayCreated nextTmid (TVarId nextVid) expiry) trace)

      ThreadDelay d k | d < 0 -> do
        let expiry    = d `addTime` time
            thread'   = thread { threadControl = ThreadControl (Return ()) (DelayFrame nextTmid k ctl) }
            simstate' = simstate { nextTmid = succ nextTmid }
        trace <- schedule thread' simstate'
        return (SimPORTrace time tid tstep tlbl (EventThreadDelay nextTmid expiry) $
                SimPORTrace time tid tstep tlbl (EventThreadDelayFired nextTmid) $
                trace)

      ThreadDelay d k -> do
        let expiry  = d `addTime` time
            timers' = IPSQ.insert (coerce nextTmid) expiry (TimerThreadDelay tid nextTmid) timers
            thread' = thread { threadControl = ThreadControl (Return ()) (DelayFrame nextTmid k ctl) }
        trace <- deschedule (Blocked BlockedOnDelay) thread'
                            simstate { timers   = timers',
                                       nextTmid = succ nextTmid }
        return (SimPORTrace time tid tstep tlbl (EventThreadDelay nextTmid expiry) trace)

      -- we treat negative timers as cancelled ones; for the record we put
      -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
      -- from `GHC.Event` behaviour.
      NewTimeout d k | d < 0 -> do
        let t       = NegativeTimeout nextTmid
            expiry  = d `addTime` time
            thread' = thread { threadControl = ThreadControl (k t) ctl }
        trace <- schedule thread' simstate { nextTmid = succ nextTmid }
        return (SimPORTrace time tid tstep tlbl (EventTimerCreated nextTmid (TVarId nextVid) expiry) $
                SimPORTrace time tid tstep tlbl (EventTimerCancelled nextTmid) $
                trace)

      NewTimeout d k -> do
        tvar  <- execNewTVar (TVarId nextVid)
                             (Just $! "<<timeout-state " ++ show (unTimeoutId nextTmid) ++ ">>")
                             TimeoutPending
        modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
        let expiry  = d `addTime` time
            t       = Timeout tvar nextTmid
            timers' = IPSQ.insert (coerce nextTmid) expiry (Timer tvar) timers
            thread' = thread { threadControl = ThreadControl (k t) ctl }
        trace <- schedule thread' simstate { timers   = timers'
                                           , nextVid  = succ (succ nextVid)
                                           , nextTmid = succ nextTmid }
        return (SimPORTrace time tid tstep tlbl (EventTimerCreated nextTmid (TVarId nextVid) expiry) trace)

      CancelTimeout (Timeout tvar tmid) k -> do
        let timers' = IPSQ.delete (coerce tmid) timers
        written <- execAtomically' (runSTM $ writeTVar tvar TimeoutCancelled)
        written' <- mapM someTVarToLabelled written
        (wakeup, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar var) -> unblockAllThreadsFromTVar var) written
        let effect' = effect
                   <> writeEffects written'
                   <> wakeupEffects wakeup
            thread' = thread { threadControl = ThreadControl k ctl
                             , threadEffect  = effect'
                             }
            (unblocked,
             simstate') = unblockThreads False vClock wakeup simstate
        modifySTRef (tvarVClock tvar)  (leastUpperBoundVClock vClock)
        !trace <- deschedule Yield thread' simstate' { timers = timers' }
        return $ SimPORTrace time tid tstep tlbl (EventTimerCancelled tmid)
               $ traceMany
                   -- TODO: step
                   [ (time, tid', (-1), tlbl', EventTxWakeup vids)
                   | tid' <- unblocked
                   , let tlbl' = lookupThreadLabel tid' threads
                   , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
               $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
               $ trace

      -- cancelling a negative timer is a no-op
      CancelTimeout (NegativeTimeout _tmid) k -> do
        -- negative timers are promptly removed from the state
        let thread' = thread { threadControl = ThreadControl k ctl }
        schedule thread' simstate

      Fork a k -> do
        let nextTId = threadNextTId thread
            tid' | threadRacy thread = setRacyThread $ childThreadId tid nextTId
                 | otherwise         = childThreadId tid nextTId
            thread'  = thread { threadControl = ThreadControl (k tid') ctl,
                                threadNextTId = nextTId + 1,
                                threadEffect  = effect
                                             <> forkEffect tid'
                                }
            thread'' = Thread { threadId      = tid'
                              , threadControl = ThreadControl (runIOSim a)
                                                              ForkFrame
                              , threadStatus  = ThreadRunning
                              , threadMasking = threadMasking thread
                              , threadThrowTo = []
                              , threadClockId = threadClockId thread
                              , threadLabel   = Nothing
                              , threadNextTId = 1
                              , threadStep    = 0
                              , threadVClock  = insertVClock tid' 0
                                              $ vClock
                              , threadEffect  = mempty
                              , threadRacy    = threadRacy thread
                              }
            threads' = Map.insert tid' thread'' threads
        -- A newly forked thread may have a higher priority, so we deschedule this one.
        !trace <- deschedule Yield thread'
                    simstate { runqueue = insertThread thread'' runqueue
                             , threads  = threads' }
        return $ SimPORTrace time tid tstep tlbl (EventThreadForked tid')
               $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
               $ trace

      Atomically a k -> execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
        case res of
          StmTxCommitted x written read created
                           tvarDynamicTraces tvarStringTraces nextVid' -> do
            (wakeup, wokeby) <- threadsUnblockedByWrites written
            mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
            vClockRead <- leastUpperBoundTVarVClocks read
            read' <- mapM someTVarToLabelled read
            written' <- mapM someTVarToLabelled written
            let vClock'     = vClock `leastUpperBoundVClock` vClockRead
                effect'     = effect
                           <> readEffects read'
                           <> writeEffects written'
                           <> wakeupEffects unblocked
                thread'     = thread { threadControl = ThreadControl (k x) ctl,
                                       threadVClock  = vClock',
                                       threadEffect  = effect' }
                (unblocked,
                 simstate') = unblockThreads True vClock' wakeup simstate
            sequence_ [ modifySTRef (tvarVClock r) (leastUpperBoundVClock vClock')
                      | SomeTVar r <- created ++ written ]
            written'' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
            created' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) created
            -- We deschedule a thread after a transaction... another may have woken up.
            !trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
            return $
              SimPORTrace time tid tstep tlbl (EventTxCommitted written'' created' (Just effect')) $
              traceMany
                [ (time, tid', (-1), tlbl', EventTxWakeup vids')
                | tid' <- unblocked
                , let tlbl' = lookupThreadLabel tid' threads
                , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ] $
              traceMany
                [ (time, tid, tstep, tlbl, EventLog tr)
                | tr <- tvarDynamicTraces
                ] $
              traceMany
                [ (time, tid, tstep, tlbl, EventSay str)
                | str <- tvarStringTraces
                ] $
              SimPORTrace time tid tstep tlbl (EventUnblocked unblocked) $
              SimPORTrace time tid tstep tlbl (EventDeschedule Yield) $
                trace

          StmTxAborted read e -> do
            -- schedule this thread to immediately raise the exception
            vClockRead <- leastUpperBoundTVarVClocks read
            read' <- mapM someTVarToLabelled read
            let effect' = effect <> readEffects read'
                thread' = thread { threadControl = ThreadControl (Throw e) ctl,
                                   threadVClock  = vClock `leastUpperBoundVClock` vClockRead,
                                   threadEffect  = effect' }
            trace <- schedule thread' simstate
            return $ SimPORTrace time tid tstep tlbl (EventTxAborted (Just effect'))
                   $ trace

          StmTxBlocked read -> do
            mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
            vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
            vClockRead <- leastUpperBoundTVarVClocks read
            read' <- mapM someTVarToLabelled read
            let effect' = effect <> readEffects read'
                thread' = thread { threadVClock  = vClock `leastUpperBoundVClock` vClockRead,
                                   threadEffect  = effect' }
            !trace <- deschedule (Blocked BlockedOnSTM) thread' simstate
            return $ SimPORTrace time tid tstep tlbl (EventTxBlocked vids (Just effect'))
                   $ SimPORTrace time tid tstep tlbl (EventDeschedule (Blocked BlockedOnSTM))
                   $ trace

      GetThreadId k -> do
        let thread' = thread { threadControl = ThreadControl (k tid) ctl }
        schedule thread' simstate

      LabelThread tid' l k | tid' == tid -> do
        let thread' = thread { threadControl = ThreadControl k ctl
                             , threadLabel   = Just l }
        schedule thread' simstate

      GetThreadLabel tid' k -> do
        let tlbl' | tid' == tid = tlbl
                  | otherwise   = tid' `Map.lookup` threads
                              >>= threadLabel
            thread' = thread { threadControl = ThreadControl (k tlbl') ctl }
        schedule thread' simstate

      LabelThread tid' l k -> do
        let thread'  = thread { threadControl = ThreadControl k ctl }
            threads' = Map.adjust (\t -> t { threadLabel = Just l }) tid' threads
        schedule thread' simstate { threads = threads' }

      ExploreRaces k -> do
        let thread'  = thread { threadControl = ThreadControl k ctl
                              , threadRacy    = True }
        schedule thread' simstate

      Fix f k -> do
        r <- newSTRef (throw NonTermination)
        x <- unsafeInterleaveST $ readSTRef r
        let k' = unIOSim (f x) $ \x' ->
                    LiftST (lazyToStrictST (writeSTRef r x')) (\() -> k x')
            thread' = thread { threadControl = ThreadControl k' ctl }
        schedule thread' simstate

      GetMaskState k -> do
        let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
        schedule thread' simstate

      SetMaskState maskst' action' k -> do
        let thread' = thread { threadControl = ThreadControl
                                                 (runIOSim action')
                                                 (MaskFrame k maskst ctl)
                             , threadMasking = maskst' }
        trace <-
          case maskst' of
            -- If we're now unmasked then check for any pending async exceptions
            Unmasked -> SimPORTrace time tid tstep tlbl (EventDeschedule Interruptable)
                    <$> deschedule Interruptable thread' simstate
            _        -> schedule                 thread' simstate
        return $ SimPORTrace time tid tstep tlbl (EventMask maskst')
               $ trace

      ThrowTo e tid' _ | tid' == tid -> do
        -- Throw to ourself is equivalent to a synchronous throw,
        -- and works irrespective of masking state since it does not block.
        let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                             , threadEffect  = effect
                             }
        trace <- schedule thread' simstate
        return (SimPORTrace time tid tstep tlbl (EventThrowTo e tid) trace)

      ThrowTo e tid' k -> do
        let thread'    = thread { threadControl = ThreadControl k ctl,
                                  threadEffect  = effect <> throwToEffect tid'
                                                         <> wakeUpEffect,
                                  threadVClock  = vClock `leastUpperBoundVClock` vClockTgt
                                }
            (vClockTgt,
             wakeUpEffect,
             willBlock) = (threadVClock t,
                           if isThreadBlocked t then wakeupEffects [tid'] else mempty,
                           not (threadInterruptible t || isThreadDone t))
              where Just t = Map.lookup tid' threads

        if willBlock
          then do
            -- The target thread has async exceptions masked so we add the
            -- exception and the source thread id to the pending async exceptions.
            let adjustTarget t =
                  t { threadThrowTo = (e, Labelled tid tlbl, vClock) : threadThrowTo t }
                threads'       = Map.adjust adjustTarget tid' threads
            trace <- deschedule (Blocked BlockedOnThrowTo) thread' simstate { threads = threads' }
            return $ SimPORTrace time tid tstep tlbl (EventThrowTo e tid')
                   $ SimPORTrace time tid tstep tlbl EventThrowToBlocked
                   $ SimPORTrace time tid tstep tlbl (EventDeschedule (Blocked BlockedOnThrowTo))
                   $ trace
          else do
            -- The target thread has async exceptions unmasked, or is masked but
            -- is blocked (and all blocking operations are interruptible) then we
            -- raise the exception in that thread immediately. This will either
            -- cause it to terminate or enter an exception handler.
            -- In the meantime the thread masks new async exceptions. This will
            -- be resolved if the thread terminates or if it leaves the exception
            -- handler (when restoring the masking state would trigger the any
            -- new pending async exception).
            let adjustTarget t@Thread{ threadControl = ThreadControl _ ctl',
                                       threadVClock  = vClock' } =
                  t { threadControl = ThreadControl (Throw e) ctl'
                    , threadStatus  = if isThreadDone t
                                      then threadStatus t
                                      else ThreadRunning
                    , threadVClock  = vClock' `leastUpperBoundVClock` vClock }
                (_unblocked, simstate'@SimState { threads = threads' }) = unblockThreads False vClock [tid'] simstate
                threads''  = Map.adjust adjustTarget tid' threads'
                simstate'' = simstate' { threads = threads'' }

            -- We yield at this point because the target thread may be higher
            -- priority, so this should be a step for race detection.
            trace <- deschedule Yield thread' simstate''
            return $ SimPORTrace time tid tstep tlbl (EventThrowTo e tid')
                   $ trace

      -- intentionally a no-op (at least for now)
      YieldSim k -> do
        let thread' = thread { threadControl = ThreadControl k ctl }
        schedule thread' simstate

      NewUnique k -> do
        let thread'   = thread{ threadControl = ThreadControl (k nextUniq) ctl }
            n         = unMkUnique nextUniq
            simstate' = simstate{ nextUniq = MkUnique (n + 1) }
        SimPORTrace time tid tstep tlbl (EventUniqueCreated n)
          <$> schedule thread' simstate'


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                   -> True
      MaskedInterruptible
        | isThreadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise              -> False
      MaskedUninterruptible      -> False


-- | Deschedule a thread.
--
-- A thread is descheduled, which marks a boundary of a `Step` when:
--
-- * forking a new thread
-- * thread termination
-- * setting the masking state to interruptible
-- * popping masking frame (which resets masking state)
-- * starting or cancelling a timeout
-- * thread delays
-- * on committed or blocked, but not aborted STM transactions
-- * on blocking or non-blocking `throwTo`
-- * unhandled exception in a (non-main) thread
--
deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)

deschedule Yield thread@Thread { threadId     = tid,
                                 threadStep   = tstep,
                                 threadLabel  = tlbl,
                                 threadVClock = vClock }
                 simstate@SimState{runqueue,
                                   threads,
                                   curTime  = time,
                                   control } =

    -- We don't interrupt runnable threads anywhere else.
    -- We do it here by inserting the current thread into the runqueue in priority order.

    let (thread', eff) = stepThread thread
        runqueue'      = insertThread thread' runqueue
        threads'       = Map.insert tid thread' threads
        control'       = advanceControl (threadStepId thread) control
        races'         = updateRaces thread simstate in

    SimPORTrace time tid tstep tlbl (EventEffect vClock eff) .
    SimPORTrace time tid tstep tlbl (EventRaces races') <$>
    reschedule simstate { runqueue = runqueue',
                          threads  = threads',
                          races    = races',
                          control  = control' }

deschedule Interruptable thread@Thread {
                           threadId      = tid,
                           threadStep    = tstep,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid', vClock') : etids,
                           threadLabel   = tlbl,
                           threadVClock  = vClock,
                           threadEffect  = effect
                         }
                        simstate@SimState{ curTime = time, threads } = do

    let effect' = effect <> wakeupEffects unblocked
        -- We're unmasking, but there are pending blocked async exceptions.
        -- So immediately raise the exception and unblock the blocked thread
        -- if possible.
        thread' = thread { threadControl = ThreadControl (Throw e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids
                         , threadVClock  = vClock `leastUpperBoundVClock` vClock'
                         , threadEffect  = effect'
                         }
        (unblocked,
         simstate') = unblockThreads False vClock [l_labelled tid'] simstate
    -- the thread is stepped when we Yield
    !trace <- deschedule Yield thread' simstate'
    return $ SimPORTrace time tid tstep tlbl (EventThrowToUnmasked tid')
           $ SimPORTrace time tid tstep tlbl (EventEffect vClock effect')
           -- TODO: step
           $ traceMany [ (time, tid'', (-1), tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
           $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
             trace

deschedule Interruptable thread@Thread{threadId     = tid,
                                       threadStep   = tstep,
                                       threadLabel  = tlbl,
                                       threadVClock = vClock}
                         simstate@SimState{ control,
                                            curTime = time } =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    -- Record a step, though, in case on replay there is an async exception.
    let (thread', eff) = stepThread thread
        races' = updateRaces thread simstate in

    SimPORTrace time tid tstep tlbl (EventEffect vClock eff) .
    SimPORTrace time tid tstep tlbl (EventRaces races') <$>
    schedule thread'
             simstate{ races   = races',
                       control = advanceControl (threadStepId thread) control }

deschedule (Blocked _blockedReason) thread@Thread { threadId      = tid
                                                  , threadStep    = tstep
                                                  , threadLabel   = tlbl
                                                  , threadThrowTo = _ : _
                                                  , threadMasking = maskst
                                                  , threadEffect  = effect }
                                    simstate@SimState{ curTime = time }
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    SimPORTrace time tid tstep tlbl (EventDeschedule Interruptable) <$>
      deschedule Interruptable thread { threadMasking = Unmasked } simstate

deschedule (Blocked blockedReason) thread@Thread{ threadId     = tid,
                                                  threadStep   = tstep,
                                                  threadLabel  = tlbl,
                                                  threadVClock = vClock}
                                   simstate@SimState{ threads,
                                                      curTime = time,
                                                      control } =
    let thread1        = thread { threadStatus = ThreadBlocked blockedReason }
        (thread', eff) = stepThread thread1
        threads'       = Map.insert (threadId thread') thread' threads
        races'         = updateRaces thread1 simstate in

    SimPORTrace time tid tstep tlbl (EventEffect vClock eff) .
    SimPORTrace time tid tstep tlbl (EventRaces races') <$>
    reschedule simstate { threads = threads',
                          races   = races',
                          control = advanceControl (threadStepId thread1) control }

deschedule Terminated thread@Thread { threadId = tid, threadStep = tstep, threadLabel = tlbl,
                                      threadVClock = vClock, threadEffect = effect }
                               simstate@SimState{ curTime = time, control } = do
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let wakeup         = map (\(_,tid',_) -> l_labelled tid') (reverse (threadThrowTo thread))
        (unblocked,
         simstate'@SimState{threads}) =
                      unblockThreads False vClock wakeup simstate
        effect'        = effect <> wakeupEffects unblocked
        (thread', eff) = stepThread $ thread { threadStatus = ThreadDone,
                                               threadEffect = effect' }
        threads'       = Map.insert tid thread' threads
        races'         = threadTerminatesRaces tid $ updateRaces thread { threadEffect = effect' } simstate
    -- We must keep terminated threads in the state to preserve their vector clocks,
    -- which matters when other threads throwTo them.
    !trace <- reschedule simstate' { races   = races',
                                     control = advanceControl (threadStepId thread) control,
                                     threads = threads' }
    return $ traceMany
               -- TODO: step
               [ (time, tid', (-1), tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
          $ SimPORTrace time tid tstep tlbl (EventEffect vClock eff)
          $ SimPORTrace time tid tstep tlbl (EventRaces races')
            trace

deschedule Sleep thread@Thread { threadId = tid , threadEffect = effect' }
                 simstate@SimState{runqueue, threads} =

    -- Schedule control says we should run a different thread. Put
    -- this one to sleep without recording a step.

    let runqueue' = insertThread thread runqueue
        threads'  = Map.insert tid thread threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }


-- Choose the next thread to run.
reschedule :: SimState s a -> ST s (SimTrace a)

-- If we are following a controlled schedule, just do that.
reschedule simstate@SimState { runqueue, control = control@(ControlFollow ((tid,_):_) _) }
                             | not (Down tid `PSQ.member` runqueue) =
    return (Trace.Nil (InternalError ("assertion failure: " ++ ppIOSimThreadId tid ++ " not runnable")))

reschedule simstate@SimState { threads, control = control@(ControlFollow ((tid,_):_) _) }
                             | not (tid `Map.member` threads) =
    return (Trace.Nil (InternalError ("assertion failure: " ++ ppIOSimThreadId tid ++ " not in threads")))

reschedule simstate@SimState { runqueue, threads,
                               control = control@(ControlFollow ((tid,tstep):_) _),
                               curTime = time } =
    fmap (SimPORTrace time tid tstep Nothing (EventReschedule control)) $
    invariant Nothing simstate $
    let thread = threads Map.! tid in
    assert (threadId thread == tid) $
    --assert (threadStep thread == tstep) $
    if threadStep thread /= tstep then
      error $ "Thread step out of sync\n"
           ++ "  runqueue:    "++show runqueue++"\n"
           ++ "  follows:     "++show tid++", step "++show tstep++"\n"
           ++ "  actual step: "++show (threadStep thread)++"\n"
           ++ "Thread:\n" ++ show thread ++ "\n"
    else
    schedule thread simstate { runqueue = PSQ.delete (Down tid) runqueue
                             , threads  = Map.delete tid threads }

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule simstate@SimState{ runqueue, threads }
    | Just (Down !tid, _, _, runqueue') <- PSQ.minView runqueue =
    invariant Nothing simstate $

    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule simstate@SimState{ threads, timers, curTime = time, races } =
    invariant Nothing simstate $

    -- time is moving on
    --Debug.trace ("Rescheduling at "++show time++", "++
      --show (length (concatMap stepInfoRaces (activeRaces races++completeRaces races)))++" races") $

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (traceFinalRacesFound simstate $
                         TraceDeadlock time (labelledThreads threads))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        written <- execAtomically' (runSTM $ mapM_ timeoutAction fired)
        !ds  <- traverse (\(SomeTVar tvar) -> do
                            tr <- traceTVarST tvar False
                            !_ <- commitTVar tvar
                            return tr) written
        (wakeupSTM, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

        let wakeupThreadDelay = [ (tid, tmid) | TimerThreadDelay tid tmid <- fired ]
            -- TODO: the vector clock below cannot be right, can it?
            !simstate'        =
                snd . unblockThreads False bottomVClock (fst `fmap` wakeupThreadDelay)
              . snd . unblockThreads True  bottomVClock wakeupSTM
              $ simstate

            -- For each 'timeout' action where the timeout has fired, start a
            -- new thread to execute throwTo to interrupt the action.
            !timeoutExpired = [ (tid, tmid, lock)
                              | TimerTimeout tid tmid lock <- fired ]

        -- all open races will be completed and reported at this time
        !simstate'' <- forkTimeoutInterruptThreads timeoutExpired
                                                   simstate' { races = noRaces }
        !trace <- reschedule simstate'' { curTime = time'
                                        , timers  = timers' }
        let traceEntries =
                     [ ( time', ThreadId [-1], -1, Just "timer"
                       , EventTimerFired tmid)
                     | (tmid, Timer _) <- zip tmids fired ]
                  ++ [ ( time', ThreadId [-1], -1, Just "register delay timer"
                       , EventRegisterDelayFired tmid)
                     | (tmid, TimerRegisterDelay _) <- zip tmids fired ]
                  ++ [ (time', ThreadId [-1], -1, Just "register delay timer", EventLog (toDyn a))
                     | TraceValue { traceDynamic = Just a } <- ds ]
                  ++ [ (time', ThreadId [-1], -1, Just "register delay timer", EventSay a)
                     | TraceValue { traceString = Just a } <- ds ]
                  ++ [ (time', tid', -1, tlbl', EventTxWakeup vids)
                     | tid' <- wakeupSTM
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
                  ++ [ ( time', tid, -1, Just "thread delay timer"
                       , EventThreadDelayFired tmid)
                     | (tid, tmid) <- wakeupThreadDelay ]
                  ++ [ ( time', tid, -1, Just "timeout timer"
                       , EventTimeoutFired tmid)
                     | (tid, tmid, _) <- timeoutExpired ]
                  ++ [ ( time', tid, -1, Just "forked thread"
                       , EventThreadForked tid)
                     | (tid, _, _) <- timeoutExpired ]

        return $
          traceFinalRacesFound simstate $
          traceMany traceEntries trace
  where
    timeoutAction (Timer var) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()
    timeoutAction (TimerRegisterDelay var) = writeTVar var True
    timeoutAction (TimerThreadDelay _ _)   = return ()
    timeoutAction (TimerTimeout _ _ _)     = return ()

unblockThreads :: forall s a.
                  Bool -- ^ `True` if we are blocked on STM
               -> VectorClock
               -> [IOSimThreadId]
               -> SimState s a
               -> ([IOSimThreadId], SimState s a)
unblockThreads !onlySTM vClock wakeup simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadBlocked is correct)
    -- we update the runqueue and threads together here
    ( unblockedIds
    , simstate { runqueue = foldr insertThread runqueue unblocked,
                 threads  = threads'
               })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    unblocked :: [Thread s a]
    !unblocked = [ thread
                 | tid <- wakeup
                 , thread <-
                     case Map.lookup tid threads of
                       Just   Thread { threadStatus = ThreadRunning }
                         -> [ ]
                       Just t@Thread { threadStatus = ThreadBlocked BlockedOnSTM }
                         -> [t]
                       Just t@Thread { threadStatus = ThreadBlocked _ }
                         | onlySTM
                         -> [ ]
                         | otherwise
                         -> [t]
                       Just   Thread { threadStatus = ThreadDone } -> [ ]
                       Nothing -> [ ]
                 ]

    unblockedIds :: [IOSimThreadId]
    !unblockedIds = map threadId unblocked

    -- and in which case we mark them as now running
    !threads'  = List.foldl'
                   (flip (Map.adjust
                     (\t -> t { threadStatus = ThreadRunning,
                                threadVClock = vClock `leastUpperBoundVClock` threadVClock t })))
                   threads unblockedIds

-- | This function receives a list of TimerTimeout values that represent threads
-- for which the timeout expired and kills the running thread if needed.
--
-- This function is responsible for the second part of the race condition issue
-- and relates to the 'schedule's 'TimeoutFrame' locking explanation (here is
-- where the assassin threads are launched. So, as explained previously, at this
-- point in code, the timeout expired so we need to interrupt the running
-- thread. If the running thread finished at the same time the timeout expired
-- we have a race condition. To deal with this race condition what we do is
-- look at the lock value. If it is 'Locked' this means that the running thread
-- already finished (or won the race) so we can safely do nothing. Otherwise, if
-- the lock value is 'NotLocked' we need to acquire the lock and launch an
-- assassin thread that is going to interrupt the running one. Note that we
-- should run this interrupting thread in an unmasked state since it might
-- receive a 'ThreadKilled' exception.
--
forkTimeoutInterruptThreads :: forall s a.
                               [(IOSimThreadId, TimeoutId, TMVar (IOSim s) IOSimThreadId)]
                            -> SimState s a
                            -> ST s (SimState s a)
forkTimeoutInterruptThreads timeoutExpired simState =
  foldlM (\st@SimState{ runqueue, threads }
           (t, TMVar lock)
          -> do
            v <- execReadTVar lock
            return $ case v of
              Nothing -> st { runqueue = insertThread t runqueue,
                              threads  = Map.insert (threadId t) t threads
                            }
              Just _  -> st
          )
          simState'
          throwToThread

  where
    -- we launch a thread responsible for throwing an AsyncCancelled exception
    -- to the thread which timeout expired
    throwToThread :: [(Thread s a, TMVar (IOSim s) IOSimThreadId)]

    (simState', throwToThread) = List.mapAccumR fn simState timeoutExpired
      where
        fn :: SimState s a
           -> (IOSimThreadId, TimeoutId, TMVar (IOSim s) IOSimThreadId)
           -> (SimState s a, (Thread s a, TMVar (IOSim s) IOSimThreadId))
        fn state@SimState { threads } (tid, tmid, lock) =
          let t = case tid `Map.lookup` threads of
                    Just t' -> t'
                    Nothing -> error ("IOSimPOR: internal error: unknown thread " ++ show tid)
              nextId   = threadNextTId t
              tid'     = childThreadId tid nextId
           in ( state { threads = Map.insert tid t { threadNextTId = succ nextId } threads }
              , ( Thread { threadId      = tid',
                           threadControl =
                            ThreadControl
                              (runIOSim $ do
                                 mtid <- myThreadId
                                 v2 <- atomically $ tryPutTMVar lock mtid
                                 when v2 $
                                   throwTo tid (toException (TimeoutException tmid)))
                              ForkFrame,
                           threadStatus  = ThreadRunning,
                           threadMasking = Unmasked,
                           threadThrowTo = [],
                           threadClockId = threadClockId t,
                           threadLabel   = Just "timeout-forked-thread",
                           threadNextTId = 1,
                           threadStep    = 0,
                           threadVClock  = insertVClock tid' 0
                                         $ threadVClock t,
                           threadEffect  = mempty,
                           threadRacy    = threadRacy t
                         }
                , lock
                )
              )


-- | Iterate through the control stack to find an enclosing exception handler
-- of the right type, or unwind all the way to the top level for the thread.
--
-- Also return if it's the main thread or a forked thread since we handle the
-- cases differently.
--
unwindControlStack :: forall s a.
                      SomeException
                   -> Thread s a
                   -> Timeouts s
                   -> ( Either Bool (Thread s a)
                      , Timeouts s
                      )
unwindControlStack e thread = \timeouts ->
    case threadControl thread of
      ThreadControl _ ctl -> unwind (threadMasking thread) ctl timeouts
  where
    unwind :: forall s' c. MaskingState
           -> ControlStack s' c a
           -> Timeouts s
           -> (Either Bool (Thread s' a), Timeouts s)
    unwind _  MainFrame                 timers = (Left True, timers)
    unwind _  ForkFrame                 timers = (Left False, timers)
    unwind _ (MaskFrame _k maskst' ctl) timers = unwind maskst' ctl timers

    unwind maskst (CatchFrame handler k ctl) timers =
      case fromException e of
        -- not the right type, unwind to the next containing handler
        Nothing -> unwind maskst ctl timers

        -- Ok! We will be able to continue the thread with the handler
        -- followed by the continuation after the catch
        Just e' -> ( Right thread {
                          -- As per async exception rules, the handler is run
                          -- masked
                         threadControl = ThreadControl (handler e')
                                                       (MaskFrame k maskst ctl),
                         threadMasking = atLeastInterruptibleMask maskst
                       }
                   , timers
                   )

    -- Either Timeout fired or the action threw an exception.
    -- - If Timeout fired, then it was possibly during this thread's execution
    --   so we need to run the continuation with a Nothing value.
    -- - If the timeout action threw an exception we need to keep unwinding the
    --   control stack looking for a handler to this exception.
    unwind maskst (TimeoutFrame tmid isLockedRef k ctl) timers =
        case fromException e of
          -- Exception came from timeout expiring
          Just (TimeoutException tmid')  | tmid == tmid' ->
            (Right thread { threadControl = ThreadControl (k Nothing) ctl }, timers')
            -- Exception came from a different exception
          _ -> unwind maskst ctl timers'
      where
        -- Remove the timeout associated with the 'TimeoutFrame'.
        timers' = IPSQ.delete (coerce tmid) timers

    unwind maskst (DelayFrame tmid _k ctl) timers =
        unwind maskst ctl timers'
      where
        -- Remove the timeout associated with the 'DelayFrame'.
        timers' = IPSQ.delete (coerce tmid) timers

    atLeastInterruptibleMask :: MaskingState -> MaskingState
    atLeastInterruptibleMask Unmasked = MaskedInterruptible
    atLeastInterruptibleMask ms       = ms


removeMinimums :: (Coercible Int k, Ord p)
               => IntPSQ p a
               -> Maybe ([k], p, [a], IntPSQ p a)
removeMinimums = \psq -> coerce $
    case IPSQ.minView psq of
      Nothing              -> Nothing
      Just (k, p, x, psq') -> Just (collectAll [k] p [x] psq')
  where
    collectAll ks p xs psq =
      case IPSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(SI.Time, IOSimThreadId, Int, Maybe ThreadLabel, SimEventType)]
          -> SimTrace a -> SimTrace a
traceMany []                                   trace = trace
traceMany ((time, tid, tstep, tlbl, event):ts) trace =
    SimPORTrace time tid tstep tlbl event (traceMany ts trace)

lookupThreadLabel :: IOSimThreadId -> Map IOSimThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'SimTrace' with 'traceResult', or access
-- 'TraceEvent's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = controlSimTraceST Nothing ControlDefault mainAction

controlSimTraceST :: Maybe Int -> ScheduleControl -> IOSim s a -> ST s (SimTrace a)
controlSimTraceST limit control mainAction =
  SimPORTrace (curTime initialState)
              (threadId mainThread)
              0
              (threadLabel mainThread)
              (EventSimStart control)
  <$> schedule mainThread initialState { control  = control,
                                         control0 = control,
                                         perStepTimeLimit = limit
                                       }
  where
    mainThread =
      Thread {
        threadId      = ThreadId [],
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadStatus  = ThreadRunning,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId [],
        threadLabel   = Just "main",
        threadNextTId = 1,
        threadStep    = 0,
        threadVClock  = insertVClock (ThreadId []) 0 bottomVClock,
        threadEffect  = mempty,
        threadRacy    = False
      }


--
-- Executing STM Transactions
--

execAtomically :: forall s a c.
                  SI.Time
               -> IOSimThreadId
               -> Maybe ThreadLabel
               -> VarId
               -> StmA s a
               -> (StmTxResult s a -> ST s (SimTrace c))
               -> ST s (SimTrace c)
execAtomically !time !tid !tlbl !nextVid0 !action0 !k0 =
    go AtomicallyFrame Map.empty Map.empty [] [] nextVid0 action0
  where
    go :: forall b.
          StmStack s b a
       -> Map TVarId (SomeTVar s)  -- set of vars read
       -> Map TVarId (SomeTVar s)  -- set of vars written
       -> [SomeTVar s]             -- vars written in order (no dups)
       -> [SomeTVar s]             -- vars created in order
       -> VarId                   -- var fresh name supply
       -> StmA s b
       -> ST s (SimTrace c)
    go !ctl !read !written !writtenSeq !createdSeq !nextVid !action =
      assert localInvariant $
      case action of
      ReturnStm x ->
        case ctl of
        AtomicallyFrame -> do
          -- Trace each created TVar
          !ds  <- traverse (\(SomeTVar tvar) -> traceTVarST tvar True) createdSeq
          -- Trace & commit each TVar
          !ds' <- Map.elems <$> traverse
                    (\(SomeTVar tvar) -> do
                        tr <- traceTVarST tvar False
                        !_ <- commitTVar tvar
                        -- Also assert the data invariant that outside a tx
                        -- the undo stack is empty:
                        undos <- readTVarUndos tvar
                        assert (null undos) $ return tr
                    ) written

          -- Return the vars written, so readers can be unblocked
          k0 $ StmTxCommitted x (reverse writtenSeq)
                                (Map.elems read)
                                (reverse createdSeq)
                                (mapMaybe (\TraceValue { traceDynamic }
                                            -> toDyn <$> traceDynamic)
                                          $ ds ++ ds')
                                (mapMaybe traceString $ ds ++ ds')
                                nextVid

        BranchFrame _b k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- The branch has successfully completed the transaction. Hence,
          -- the alternative branch can be ignored.
          -- Commit the TVars written in this sub-transaction that are also
          -- in the written set of the outer transaction
          !_ <- traverse_ (\(SomeTVar tvar) -> commitTVar tvar)
                          (Map.intersection written writtenOuter)
          -- Merge the written set of the inner with the outer
          let written'    = Map.union written writtenOuter
              writtenSeq' = filter (\(SomeTVar tvar) ->
                                      tvarId tvar `Map.notMember` writtenOuter)
                                    writtenSeq
                         ++ writtenOuterSeq
              createdSeq' = createdSeq ++ createdOuterSeq
          -- Skip the orElse right hand and continue with the k continuation
          go ctl' read written' writtenSeq' createdSeq' nextVid (k x)

      ThrowStm e -> do
        -- Revert all the TVar writes
        !_ <- traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        case ctl of
          AtomicallyFrame -> do
            k0 $ StmTxAborted (Map.elems read) (toException e)

          BranchFrame (CatchStmA h) k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
            -- Execute the left side in a new frame with an empty written set.
            -- but preserve ones that were set prior to it, as specified in the
            -- [stm](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#v:catchSTM) package.
            let ctl'' = BranchFrame NoOpStmA k writtenOuter writtenOuterSeq createdOuterSeq ctl'
            go ctl'' read Map.empty [] [] nextVid (h e)

          BranchFrame (OrElseStmA _r) _k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid (ThrowStm e)

          BranchFrame NoOpStmA _k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid (ThrowStm e)

      CatchStm a h k -> do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = BranchFrame (CatchStmA h) k written writtenSeq createdSeq ctl
        go ctl' read Map.empty [] [] nextVid a

      Retry -> do
        -- Always revert all the TVar writes for the retry
        !_ <- traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        case ctl of
          AtomicallyFrame -> do
            -- Return vars read, so the thread can block on them
            k0 $! StmTxBlocked $! Map.elems read

          BranchFrame (OrElseStmA b) k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
            -- Execute the orElse right hand with an empty written set
            let ctl'' = BranchFrame NoOpStmA k writtenOuter writtenOuterSeq createdOuterSeq ctl'
            go ctl'' read Map.empty [] [] nextVid b

          BranchFrame _ _k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
            -- Retry makes sense only within a OrElse context. If it is a branch other than
            -- OrElse left side, then bubble up the `retry` to the frame above.
            -- Skip the continuation and propagate the retry into the outer frame
            -- using the written set for the outer frame
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid Retry

      OrElse a b k -> do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = BranchFrame (OrElseStmA b) k written writtenSeq createdSeq ctl
        go ctl' read Map.empty [] [] nextVid a

      NewTVar mkId !mbLabel x k -> do
        !v <- execNewTVar (mkId nextVid) mbLabel x
        -- record a write to the TVar so we know to update its VClock
        let written' = Map.insert (tvarId v) (SomeTVar v) written
        -- save the value: it will be committed or reverted
        !_ <- saveTVar v
        go ctl read written' writtenSeq (SomeTVar v : createdSeq) (succ nextVid) (k v)

      LabelTVar !label tvar k -> do
        !_ <- writeSTRef (tvarLabel tvar) $! (Just label)
        go ctl read written writtenSeq createdSeq nextVid k

      TraceTVar tvar f k -> do
        !_ <- writeSTRef (tvarTrace tvar) (Just f)
        go ctl read written writtenSeq createdSeq nextVid k

      ReadTVar v k
        | tvarId v `Map.member` read -> do
            x <- execReadTVar v
            go ctl read written writtenSeq createdSeq nextVid (k x)
        | otherwise -> do
            x <- execReadTVar v
            let read' = Map.insert (tvarId v) (SomeTVar v) read
            go ctl read' written writtenSeq createdSeq nextVid (k x)

      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            !_ <- execWriteTVar v x
            go ctl read written writtenSeq createdSeq nextVid k
        | otherwise -> do
            !_ <- saveTVar v
            !_ <- execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go ctl read written' (SomeTVar v : writtenSeq) createdSeq nextVid k

      SayStm msg k -> do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        -- TODO: step
        return $ SimPORTrace time tid (-1) tlbl (EventSay msg) trace

      OutputStm x k -> do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        -- TODO: step
        return $ SimPORTrace time tid (-1) tlbl (EventLog x) trace

      LiftSTStm st k -> do
        x <- strictToLazyST st
        go ctl read written writtenSeq createdSeq nextVid (k x)

      FixStm f k -> do
        r <- newSTRef (throw NonTermination)
        x <- unsafeInterleaveST $ readSTRef r
        let k' = unSTM (f x) $ \x' ->
                    LiftSTStm (lazyToStrictST (writeSTRef r x')) (\() -> k x')
        go ctl read written writtenSeq createdSeq nextVid k'

      where
        localInvariant =
            Map.keysSet written
         == Set.fromList ([ tvarId tvar | SomeTVar tvar <- writtenSeq ]
                       ++ [ tvarId tvar | SomeTVar tvar <- createdSeq ])


-- | Special case of 'execAtomically' supporting only var reads and writes
--
execAtomically' :: StmA s () -> ST s [SomeTVar s]
execAtomically' = go Map.empty
  where
    go :: Map TVarId (SomeTVar s)  -- set of vars written
       -> StmA s ()
       -> ST s [SomeTVar s]
    go !written action = case action of
      ReturnStm () -> do
        return (Map.elems written)
      ReadTVar v k  -> do
        x <- execReadTVar v
        go written (k x)
      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            !_ <- execWriteTVar v x
            go written k
        | otherwise -> do
            !_ <- saveTVar v
            !_ <- execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go written' k
      _ -> error "execAtomically': only for special case of reads and writes"


execNewTVar :: TVarId -> Maybe String -> a -> ST s (TVar s a)
execNewTVar !tvarId !mbLabel x = do
    tvarLabel   <- newSTRef mbLabel
    tvarCurrent <- newSTRef x
    tvarUndo    <- newSTRef []
    tvarBlocked <- newSTRef ([], Set.empty)
    tvarVClock  <- newSTRef bottomVClock
    tvarTrace   <- newSTRef Nothing
    return TVar {tvarId, tvarLabel,
                 tvarCurrent, tvarUndo, tvarBlocked, tvarVClock,
                 tvarTrace}

-- 'execReadTVar' is defined in `Control.Monad.IOSim.Type` and shared with /IOSim/

execWriteTVar :: TVar s a -> a -> ST s ()
execWriteTVar TVar{tvarCurrent} = writeSTRef tvarCurrent
{-# INLINE execWriteTVar #-}

execTryPutTMVar :: TMVar (IOSim s) a -> a -> ST s Bool
execTryPutTMVar (TMVar var) a = do
    v <- execReadTVar var
    case v of
      Nothing -> execWriteTVar var (Just a)
              >> return True
      Just _  -> return False
{-# INLINE execTryPutTMVar #-}

saveTVar :: TVar s a -> ST s ()
saveTVar TVar{tvarCurrent, tvarUndo} = do
    -- push the current value onto the undo stack
    v   <- readSTRef tvarCurrent
    !vs <- readSTRef tvarUndo
    writeSTRef tvarUndo $! v:vs

revertTVar :: TVar s a -> ST s ()
revertTVar TVar{tvarCurrent, tvarUndo} = do
    -- pop the undo stack, and revert the current value
    !vs <- readSTRef tvarUndo
    !_  <- writeSTRef tvarCurrent (head vs)
    writeSTRef tvarUndo $! tail vs
{-# INLINE revertTVar #-}

commitTVar :: TVar s a -> ST s ()
commitTVar TVar{tvarUndo} = do
    !vs <- readSTRef tvarUndo
    -- pop the undo stack, leaving the current value unchanged
    writeSTRef tvarUndo $! tail vs
{-# INLINE commitTVar #-}

readTVarUndos :: TVar s a -> ST s [a]
readTVarUndos TVar{tvarUndo} = readSTRef tvarUndo

-- | Trace a 'TVar'.  It must be called only on 'TVar's that were new or
-- 'written.
traceTVarST :: TVar s a
            -> Bool -- true if it's a new 'TVar'
            -> ST s TraceValue
traceTVarST TVar{tvarCurrent, tvarUndo, tvarTrace} new = do
    mf <- readSTRef tvarTrace
    case mf of
      Nothing -> return TraceValue { traceDynamic = (Nothing :: Maybe ()), traceString = Nothing }
      Just f  -> do
        !vs <- readSTRef tvarUndo
        v   <- readSTRef tvarCurrent
        case (new, vs) of
          (True, _) -> f Nothing v
          (_, _:_)  -> f (Just $ last vs) v
          _         -> error "traceTVarST: unexpected tvar state"



leastUpperBoundTVarVClocks :: [SomeTVar s] -> ST s VectorClock
leastUpperBoundTVarVClocks tvars =
  foldr leastUpperBoundVClock bottomVClock <$>
    sequence [readSTRef (tvarVClock r) | SomeTVar r <- tvars]

--
-- Blocking and unblocking on TVars
--

readTVarBlockedThreads :: TVar s a -> ST s [IOSimThreadId]
readTVarBlockedThreads TVar{tvarBlocked} = fst <$> readSTRef tvarBlocked

blockThreadOnTVar :: IOSimThreadId -> TVar s a -> ST s ()
blockThreadOnTVar tid TVar{tvarBlocked} = do
    (tids, tidsSet) <- readSTRef tvarBlocked
    when (tid `Set.notMember` tidsSet) $ do
      let !tids'    = tid : tids
          !tidsSet' = Set.insert tid tidsSet
      writeSTRef tvarBlocked (tids', tidsSet')

unblockAllThreadsFromTVar :: TVar s a -> ST s ()
unblockAllThreadsFromTVar TVar{tvarBlocked} = do
    writeSTRef tvarBlocked ([], Set.empty)

-- | For each TVar written to in a transaction (in order) collect the threads
-- that blocked on each one (in order).
--
-- Also, for logging purposes, return an association between the threads and
-- the var writes that woke them.
--
threadsUnblockedByWrites :: [SomeTVar s]
                         -> ST s ([IOSimThreadId], Map IOSimThreadId (Set (Labelled TVarId)))
threadsUnblockedByWrites written = do
  tidss <- sequence
             [ (,) <$> labelledTVarId tvar <*> readTVarBlockedThreads tvar
             | SomeTVar tvar <- written ]
  -- Threads to wake up, in wake up order, annotated with the vars written that
  -- caused the unblocking.
  -- We reverse the individual lists because the tvarBlocked is used as a stack
  -- so it is in order of last written, LIFO, and we want FIFO behaviour.
  let wakeup = ordNub [ tid | (_vid, tids) <- tidss, tid <- reverse tids ]
      wokeby = Map.fromListWith Set.union
                                [ (tid, Set.singleton vid)
                                | (vid, tids) <- tidss
                                , tid <- tids ]
  return (wakeup, wokeby)

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `Set.member` s = go s xs
      | otherwise        = x : go (Set.insert x s) xs

--
-- Steps
--

-- | Check if two steps can be reordered with a possibly different outcome.
--
racingSteps :: Step -- ^ an earlier step
            -> Step -- ^ a later step
            -> Bool
racingSteps s s' =
     -- is s executed by a racy thread
     isRacyThreadId (stepThreadId s)
     -- steps which belong to the same thread cannot race
  && stepThreadId s /= stepThreadId s'
     -- if s wakes up s' then s and s' cannot race
  && not (stepThreadId s' `elem` effectWakeup (stepEffect s))
     -- s effects races with s' effects or either one throws to the other
  && (  stepEffect s `racingEffects` stepEffect s'
     || throwsTo s s'
     || throwsTo s' s
     )
  where throwsTo s1 s2 =
             stepThreadId s2 `elem` effectThrows (stepEffect s1)
          -- `throwTo` races with any other effect
          && stepEffect s2 /= mempty

currentStep :: Thread s a -> Step
currentStep Thread { threadId     = stepThreadId,
                     threadStep   = stepStep,
                     threadEffect = stepEffect,
                     threadVClock = stepVClock
                   } = Step {..}

-- | Step a thread and return the previous `StepId` and its `Effect`.
--
stepThread :: Thread s a -> (Thread s a, Effect)
stepThread thread@Thread { threadId     = tid,
                           threadStep   = tstep,
                           threadVClock = vClock } =
  ( thread { threadStep   = tstep+1,
             threadEffect = mempty,
             threadVClock = insertVClock tid (tstep+1) vClock
           },
    threadEffect thread
  )

-- | 'updateRaces' turns a current 'Step' into 'StepInfo', and updates all
-- 'activeRaces'.
--
-- We take care that steps can only race against threads in their
-- concurrent set. When this becomes empty, a step can be retired into
-- the "complete" category, but only if there are some steps racing
-- with it.
updateRaces :: Thread s a -> SimState s a -> Races
updateRaces thread@Thread { threadId = tid }
            SimState{ control, threads, races = races@Races { activeRaces } } =
    let
        newStep@Step{ stepEffect = newEffect } = currentStep thread

        concurrent0 =
          Map.keysSet (Map.filter (\t -> not (isThreadDone t)
                                      && threadId t `Set.notMember`
                                         effectForks newEffect
                                  ) threads)

        -- A new step to add to the `activeRaces` list.
        newStepInfo :: Maybe StepInfo
        !newStepInfo | isNotRacyThreadId tid = Nothing
                       -- non-racy threads do not race

                     | Set.null concurrent   = Nothing
                       -- cannot race with nothing

                     | isBlocking            = Nothing
                     -- no need to defer a blocking transaction

                     | otherwise =
            Just $! StepInfo { stepInfoStep       = newStep,
                               stepInfoControl    = control,
                               stepInfoConcurrent = concurrent,
                               stepInfoNonDep     = [],
                               stepInfoRaces      = []
                             }
          where
            concurrent :: Set IOSimThreadId
            concurrent = concurrent0 Set.\\ effectWakeup newEffect

            isBlocking :: Bool
            isBlocking = isThreadBlocked thread && onlyReadEffect newEffect

        -- Used to update each `StepInfo` in `activeRaces`.
        updateStepInfo :: StepInfo -> StepInfo
        updateStepInfo stepInfo@StepInfo { stepInfoStep       = step,
                                           stepInfoConcurrent = concurrent,
                                           stepInfoNonDep,
                                           stepInfoRaces  } =
          -- if this step depends on the previous step, or is not concurrent,
          -- then any threads that it wakes up become non-concurrent also.
          let !lessConcurrent = concurrent Set.\\ effectWakeup newEffect

              -- `step` happened before `newStep` (`newStep` happened after
              -- `step`)
              happensBefore   = step `happensBeforeStep` newStep

              !stepInfoNonDep'
                -- `newStep` happened after `step`
                | happensBefore =           stepInfoNonDep
                -- `newStep` did not happen after `step`
                | otherwise     = newStep : stepInfoNonDep in

          if tid `notElem` concurrent
            then let
                  in stepInfo { stepInfoConcurrent = lessConcurrent
                              , stepInfoNonDep     = stepInfoNonDep'
                              }

            -- The core of IOSimPOR.  Detect if `newStep` is racing with any
            -- previous steps and update each `StepInfo`.
            else let theseStepsRace = step `racingSteps` newStep
                     -- `newStep` happens after any of the racing steps
                     afterRacingStep = any (`happensBeforeStep` newStep) stepInfoRaces

                     -- We will only record the first race with each thread.
                     -- Reversing the first race makes the next race detectable.
                     -- Thus we remove a thread from the concurrent set after the
                     -- first race.
                     !concurrent'
                       | happensBefore   = Set.delete tid lessConcurrent
                       | theseStepsRace  = Set.delete tid concurrent
                       | afterRacingStep = Set.delete tid concurrent
                       | otherwise       = concurrent

                     -- Here we record discovered races.  We only record a new
                     -- race if we are following the default schedule, to avoid
                     -- finding the same race in different parts of the search
                     -- space.
                     !stepInfoRaces'
                       | theseStepsRace && isDefaultSchedule control
                                   = newStep : stepInfoRaces
                       | otherwise =           stepInfoRaces

            in stepInfo { stepInfoConcurrent = effectForks newEffect
                                               `Set.union` concurrent',
                          stepInfoNonDep     = stepInfoNonDep',
                          stepInfoRaces      = stepInfoRaces'
                        }

        activeRaces' :: [StepInfo]
        !activeRaces' =
          case newStepInfo of
            Nothing ->       updateStepInfo <$> activeRaces
            Just si -> si : (updateStepInfo <$> activeRaces)

    in normalizeRaces races { activeRaces = activeRaces' }


normalizeRaces :: Races -> Races
normalizeRaces Races{ activeRaces, completeRaces } =
  let !activeRaces'   =   filter (not . null . stepInfoConcurrent) activeRaces
      !completeRaces' = ( filter (not . null . stepInfoRaces)
                        . filter (null . stepInfoConcurrent)
                        $ activeRaces
                        )
                     ++ completeRaces
  in Races{ activeRaces   = activeRaces',
            completeRaces = completeRaces' }


-- When a thread terminates, we remove it from the concurrent thread
-- sets of active races.
threadTerminatesRaces :: IOSimThreadId -> Races -> Races
threadTerminatesRaces tid races@Races{ activeRaces } =
  let activeRaces' = [ s{stepInfoConcurrent = Set.delete tid stepInfoConcurrent}
                     | s@StepInfo{ stepInfoConcurrent } <- activeRaces ]
  in normalizeRaces $ races{ activeRaces = activeRaces' }

-- We assume that steps do not race with later steps after a quiescent
-- period. Quiescent periods end when simulated time advances, thus we
-- are assuming here that all work is completed before a timer
-- triggers.

quiescentRaces :: Races -> Races
quiescentRaces Races{ activeRaces, completeRaces } =
  Races{ activeRaces = [],
         completeRaces = [ s{stepInfoConcurrent = Set.empty}
                         | s <- activeRaces
                         , not (null (stepInfoRaces s))
                         ] ++ completeRaces }


--
-- Schedule control
--

controlTargets :: StepId -> ScheduleControl -> Bool
controlTargets stepId
               (ControlAwait (ScheduleMod{ scheduleModTarget }:_)) =
  stepId == scheduleModTarget
controlTargets _stepId _ = False

followControl :: ScheduleControl -> ScheduleControl
followControl (ControlAwait (ScheduleMod { scheduleModInsertion } : mods)) =
               ControlFollow scheduleModInsertion mods
followControl (ControlAwait []) = error "Impossible: followControl (ControlAwait [])"
followControl ControlDefault{}  = error "Impossible: followControl ControlDefault{}"
followControl ControlFollow{}   = error "Impossible: followControl ControlFollow{}"

controlFollows :: StepId -> ScheduleControl -> Bool
controlFollows _stepId  ControlDefault               = True
controlFollows _stepId (ControlFollow [] _)          = True
controlFollows stepId  (ControlFollow (stepId':_) _) = stepId == stepId'
controlFollows stepId  (ControlAwait (smod:_))       = stepId /= scheduleModTarget smod
controlFollows _       (ControlAwait [])             = error "Impossible: controlFollows _ (ControlAwait [])"

advanceControl :: StepId -> ScheduleControl -> ScheduleControl
advanceControl (tid,step) control@(ControlFollow ((tid',step'):sids') tgts)
  | tid /= tid' =
      -- we are switching threads to follow the schedule
      --Debug.trace ("Switching threads from "++show (tid,step)++" to "++show (tid',step')++"\n") $
      control
  | step == step' =
      ControlFollow sids' tgts
  | otherwise =
      error $ concat
            [ "advanceControl ", show (tid,step)
            , " cannot follow step ", show step'
            , "\n"
            ]
advanceControl stepId (ControlFollow [] []) =
  ControlDefault
advanceControl stepId (ControlFollow [] tgts) =
  ControlAwait tgts
advanceControl stepId control =
  assert (not $ controlTargets stepId control) $
  control

--
-- Schedule modifications
--

stepStepId :: Step -> (IOSimThreadId, Int)
stepStepId Step{ stepThreadId = tid, stepStep = n } = (tid,n)

stepInfoToScheduleMods :: StepInfo -> [ScheduleMod]
stepInfoToScheduleMods
  StepInfo{ stepInfoStep    = step,
            stepInfoControl = control,
            stepInfoNonDep  = nondep,
            stepInfoRaces   = races
          } =
  -- It is actually possible for a later step that races with an earlier one
  -- not to *depend* on it in a happens-before sense. But we don't want to try
  -- to follow any steps *after* the later one.
  [ ScheduleMod
      { scheduleModTarget    = stepStepId step
      , scheduleModControl   = control
      , scheduleModInsertion = takeWhile (/=stepStepId step')
                                         (stepStepId `map` reverse nondep)
                            ++ [stepStepId step']
                            -- It should be unnecessary to include the delayed
                            -- step in the insertion, since the default
                            -- scheduling should run it anyway. Removing it may
                            -- help avoid redundant schedules.
                            -- ++ [stepStepId step]
      }
  | step' <- races ]

traceFinalRacesFound :: SimState s a -> SimTrace a -> SimTrace a
traceFinalRacesFound SimState{ control0 = control, races } =
    TraceRacesFound [extendScheduleControl control m | m <- scheduleMods]
  where
    scheduleMods :: [ScheduleMod]
    scheduleMods =
        concatMap stepInfoToScheduleMods
      . completeRaces
      . quiescentRaces
      $ races

-- Extend an existing schedule control with a newly discovered schedule mod
extendScheduleControl' :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl' ControlDefault m = ControlAwait [m]
extendScheduleControl' (ControlAwait mods) m =
  case scheduleModControl m of
    ControlDefault     -> ControlAwait (mods++[m])
    ControlAwait mods' ->
      let common = length mods - length mods' in
      assert (common >= 0 && drop common mods==mods') $
      ControlAwait (take common mods++[m{ scheduleModControl = ControlDefault }])
    ControlFollow stepIds mods' ->
      let common = length mods - length mods' - 1
          m'     = mods !! common
          isUndo = scheduleModTarget m' `elem` scheduleModInsertion m
          m''    = m'{ scheduleModInsertion =
                         takeWhile (/=scheduleModTarget m)
                                   (scheduleModInsertion m')
                         ++
                         scheduleModInsertion m }
      in
      assert (common >= 0) $
      assert (drop (common+1) mods == mods') $
      if isUndo
        then ControlAwait mods          -- reject this mod... it's undoing a previous one
        else ControlAwait (take common mods++[m''])
extendScheduleControl' ControlFollow{} ScheduleMod{} =
  -- note: this case is impossible, since `extendScheduleControl'` first
  -- argument is either the initial `ControlDefault` or a result of calling
  -- `extendScheduleControl'` itself.
  error "Impossible: extendScheduleControl' ControlFollow{} ScheduleMod{}"

extendScheduleControl :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl control m =
  let control' = extendScheduleControl' control m in
  {- Debug.trace (unlines ["",
                        "Extending "++show control,
                        "     with "++show m,
                        "   yields "++show control']) -}
  control'
