{-# LANGUAGE CPP          #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#if __GLASGOW_HASKELL__ >= 908
-- We use partial functions from `Data.List`.
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Control.Monad.IOSim.Internal
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
  , Trace.Trace (SimTrace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , SimEvent (..)
  , SimResult (..)
  , SimEventType (..)
  , ppTrace
  , ppTrace_
  , ppSimEvent
  , liftST
  , execReadTVar
  ) where

import Prelude hiding (read)

import Data.Coerce
import Data.Deque.Strict (Deque)
import Data.Deque.Strict qualified as Deque
import Data.Dynamic
import Data.Foldable (foldlM, toList, traverse_)
import Data.IntPSQ (IntPSQ)
import Data.IntPSQ qualified as PSQ
import Data.List qualified as List
import Data.List.Trace qualified as Trace
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
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
import Control.Monad.Class.MonadThrow hiding (getMaskingState)
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer.SI (TimeoutState (..))

import Control.Monad.IOSim.InternalTypes
import Control.Monad.IOSim.Types hiding (SimEvent (SimPOREvent),
           Trace (SimPORTrace))
import Control.Monad.IOSim.Types (SimEvent)

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !IOSimThreadId,
    threadControl :: !(ThreadControl s a),
    threadStatus  :: !ThreadStatus,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled IOSimThreadId)],
    threadClockId :: !ClockId,
    threadLabel   ::  Maybe ThreadLabel,
    threadNextTId :: !Int
  }

isThreadBlocked :: Thread s a -> Bool
isThreadBlocked t = case threadStatus t of
    ThreadBlocked {} -> True
    _                -> False

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = Labelled tvarId <$> readSTRef tvarLabel

labelledThreads :: Map IOSimThreadId (Thread s a) -> [Labelled IOSimThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not ratain the
    -- original thread map we need to evaluate the spine of the list.
    -- TODO: https://github.com/haskell/containers/issues/749
    Map.foldr'
      (\Thread { threadId, threadLabel } !acc -> Labelled threadId threadLabel : acc)
      [] threadMap


-- | Timers mutable variables. Supports 'newTimeout' api, the second
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


type Timeouts s = IntPSQ Time (TimerCompletionInfo s)

-- | Internal state.
--
data SimState s a = SimState {
       runqueue :: !(Deque IOSimThreadId),
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads  :: !(Map IOSimThreadId (Thread s a)),
       -- | current time
       curTime  :: !Time,
       -- | ordered list of timers and timeouts
       timers   :: !(Timeouts s),
       -- | list of clocks
       clocks   :: !(Map ClockId UTCTime),
       nextVid  :: !VarId,     -- ^ next unused 'VarId'
       nextTmid :: !TimeoutId, -- ^ next unused 'TimeoutId'
       nextUniq :: !(Unique s) -- ^ next unused @'Unique' s@
     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = mempty,
      threads  = Map.empty,
      curTime  = Time 0,
      timers   = PSQ.empty,
      clocks   = Map.singleton (ClockId []) epoch1970,
      nextVid  = 0,
      nextTmid = TimeoutId 0,
      nextUniq = MkUnique 0
    }
  where
    epoch1970 = UTCTime (fromGregorian 1970 1 1) 0

invariant :: Maybe (Thread s a) -> SimState s a -> x -> x

invariant (Just running) simstate@SimState{runqueue,threads,clocks} =
   assert (not (isThreadBlocked running))
 . assert (threadId running `Map.notMember` threads)
 . assert (threadId running `List.notElem` runqueue)
 . assert (threadClockId running `Map.member` clocks)
 . invariant Nothing simstate

invariant Nothing SimState{runqueue,threads,clocks} =
   assert (all (`Map.member` threads) runqueue)
 . assert (and [ isThreadBlocked t == (threadId t `notElem` runqueue)
               | t <- Map.elems threads ])
 . assert (toList runqueue == List.nub (toList runqueue))
 . assert (and [ threadClockId t `Map.member` clocks
               | t <- Map.elems threads ])

-- | Interpret the simulation monotonic time as a 'NominalDiffTime' since
-- the start.
timeSinceEpoch :: Time -> NominalDiffTime
timeSinceEpoch (Time t) = fromRational (toRational t)


-- | Schedule / run a thread.
--
schedule :: forall s a. Thread s a -> SimState s a -> ST s (SimTrace a)
schedule !thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst,
           threadLabel   = tlbl
         }
         !simstate@SimState {
           runqueue,
           threads,
           timers,
           clocks,
           nextVid, nextTmid, nextUniq,
           curTime  = time
         } =
  invariant (Just thread) simstate $
  case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ SimTrace time tid tlbl EventThreadFinished
               $ TraceMainReturn time (Labelled tid tlbl) x (labelledThreads threads)

      ForkFrame -> do
        -- this thread is done
        !trace <- deschedule Terminated thread simstate
        return $ SimTrace time tid tlbl EventThreadFinished
               $ SimTrace time tid tlbl (EventDeschedule Terminated)
               $ trace

      MaskFrame k maskst' ctl' -> do
        -- pop the control stack, restore thread-local state
        let thread' = thread { threadControl = ThreadControl (k x) ctl'
                             , threadMasking = maskst' }
        -- but if we're now unmasked, check for any pending async exceptions
        !trace <- deschedule Interruptable thread' simstate
        return $ SimTrace time tid tlbl (EventMask maskst')
               $ SimTrace time tid tlbl (EventDeschedule Interruptable)
               $ trace

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

      TimeoutFrame tmid lock k ctl' -> do
        -- There is a possible race between timeout action and the timeout expiration.
        -- We use a lock to solve the race.

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
            timers' = (PSQ.delete . coerce) tmid timers
        schedule thread' simstate { timers = timers' }

    Throw e -> case unwindControlStack e thread timers of
      -- Found a CatchFrame
      (Right thread'@Thread { threadMasking = maskst' }, timers'') -> do
        -- We found a suitable exception handler, continue with that
        trace <- schedule thread' simstate { timers = timers'' }
        return (SimTrace time tid tlbl (EventThrow e) $
                SimTrace time tid tlbl (EventMask maskst') trace)

      (Left isMain, timers'')
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (SimTrace time tid tlbl (EventThrow e) $
                  SimTrace time tid tlbl (EventThreadUnhandled e) $
                  TraceMainException time (Labelled tid tlbl) e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          !trace <- deschedule Terminated thread simstate { timers = timers'' }
          return $ SimTrace time tid tlbl (EventThrow e)
                 $ SimTrace time tid tlbl (EventThreadUnhandled e)
                 $ SimTrace time tid tlbl (EventDeschedule Terminated)
                 $ trace

    Catch action' handler k -> do
      -- push the failure and success continuations onto the control stack
      let thread' = thread { threadControl = ThreadControl action'
                                               (CatchFrame handler k ctl) }
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
          return $ SimTrace time tid tlbl (EventEvaluationError e)
                 $ trace
        Right whnf -> do
          -- continue with the resulting WHNF
          let thread' = thread { threadControl = ThreadControl (k whnf) ctl }
          trace <- schedule thread' simstate
          return $ SimTrace time tid tlbl EventEvaluationSuccess
                 $ trace

    Say msg k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventSay msg) trace)

    Output x k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventLog x) trace)

    LiftST st k -> do
      x <- strictToLazyST st
      let thread' = thread { threadControl = ThreadControl (k x) ctl }
      schedule thread' simstate

    GetMonoTime k -> do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    GetWallTime k -> do
      let !clockid  = threadClockId thread
          !clockoff = clocks Map.! clockid
          !walltime = timeSinceEpoch time `addUTCTime` clockoff
          !thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
      schedule thread' simstate

    SetWallTime walltime' k -> do
      let !clockid   = threadClockId thread
          !clockoff  = clocks Map.! clockid
          !walltime  = timeSinceEpoch time `addUTCTime` clockoff
          !clockoff' = addUTCTime (diffUTCTime walltime' walltime) clockoff
          !thread'   = thread { threadControl = ThreadControl k ctl }
          !simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
      schedule thread' simstate'

    UnshareClock k -> do
      let !clockid   = threadClockId thread
          !clockoff  = clocks Map.! clockid
          !clockid'  = let ThreadId i = tid in ClockId i -- reuse the thread id
          !thread'   = thread { threadControl = ThreadControl k ctl
                              , threadClockId = clockid' }
          !simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
      schedule thread' simstate'

    -- This case is guarded by checks in 'timeout' itself.
    StartTimeout d _ _ | d <= 0 ->
      error "schedule: StartTimeout: Impossible happened"

    StartTimeout d action' k -> do
      !lock <- TMVar <$> execNewTVar (TMVarId nextVid) (Just $! "lock-" ++ show nextTmid) Nothing
      let !expiry    = d `addTime` time
          !timers'   = (PSQ.insert . coerce) nextTmid expiry (TimerTimeout tid nextTmid lock) timers
          !thread'   = thread { threadControl =
                                 ThreadControl action'
                                               (TimeoutFrame nextTmid lock k ctl)
                              }
      !trace <- deschedule Yield thread' simstate { timers   = timers'
                                                  , nextTmid = succ nextTmid
                                                  , nextVid  = succ nextVid
                                                  }
      return (SimTrace time tid tlbl (EventTimeoutCreated nextTmid tid expiry) trace)

    UnregisterTimeout tmid k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate { timers = (PSQ.delete . coerce) tmid timers }

    RegisterDelay d k | d < 0 -> do
      !tvar <- execNewTVar (TVarId nextVid)
                          (Just $! "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          True
      let !expiry  = d `addTime` time
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { nextVid = succ nextVid }
      return (SimTrace time tid tlbl (EventRegisterDelayCreated nextTmid (TVarId nextVid) expiry) $
              SimTrace time tid tlbl (EventRegisterDelayFired nextTmid) $
              trace)

    RegisterDelay d k -> do
      !tvar <- execNewTVar (TVarId nextVid)
                          (Just $! "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          False
      let !expiry  = d `addTime` time
          !timers' = (PSQ.insert . coerce) nextTmid expiry (TimerRegisterDelay tvar) timers
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl
                (EventRegisterDelayCreated nextTmid (TVarId nextVid) expiry) trace)

    ThreadDelay d k | d < 0 -> do
      let !expiry    = d `addTime` time
          !thread'   = thread { threadControl = ThreadControl (Return ()) (DelayFrame nextTmid k ctl) }
          !simstate' = simstate { nextTmid = succ nextTmid }
      trace <- schedule thread' simstate'
      return (SimTrace time tid tlbl (EventThreadDelay nextTmid expiry) $
              SimTrace time tid tlbl (EventThreadDelayFired nextTmid) $
              trace)

    ThreadDelay d k -> do
      let !expiry  = d `addTime` time
          !timers' = (PSQ.insert . coerce) nextTmid expiry (TimerThreadDelay tid nextTmid) timers
          !thread' = thread { threadControl = ThreadControl (Return ()) (DelayFrame nextTmid k ctl) }
      !trace <- deschedule (Blocked BlockedOnDelay) thread' simstate { timers   = timers'
                                                                     , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventThreadDelay nextTmid expiry) trace)

    -- we treat negative timers as cancelled ones; for the record we put
    -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
    -- from `GHC.Event` behaviour.
    NewTimeout d k | d < 0 -> do
      let !t       = NegativeTimeout nextTmid
          !expiry  = d `addTime` time
          !thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid (TVarId nextVid) expiry) $
              SimTrace time tid tlbl (EventTimerCancelled nextTmid) $
              trace)

    NewTimeout d k -> do
      !tvar  <- execNewTVar (TVarId nextVid)
                           (Just $! "<<timeout-state " ++ show (unTimeoutId nextTmid) ++ ">>")
                           TimeoutPending
      let !expiry  = d `addTime` time
          !t       = Timeout tvar nextTmid
          !timers' = (PSQ.insert . coerce) nextTmid expiry (Timer tvar) timers
          !thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid (TVarId nextVid) expiry) trace)

    CancelTimeout (Timeout tvar tmid) k -> do
      let !timers' = (PSQ.delete . coerce) tmid timers
          !thread' = thread { threadControl = ThreadControl k ctl }
      !written <- execAtomically' (runSTM $ writeTVar tvar TimeoutCancelled)
      -- note: we are not running traceTVar on 'tvar', since its not exposed to
      -- the user, and thus it cannot have an attached callback.
      !_ <- traverse_ (\(SomeTVar tvar') -> commitTVar tvar') written
      (wakeup, wokeby) <- threadsUnblockedByWrites written
      mapM_ (\(SomeTVar var) -> unblockAllThreadsFromTVar var) written
      let (unblocked,
           simstate') = unblockThreads True wakeup simstate
      trace <- schedule thread' simstate' { timers = timers' }
      return $ SimTrace time tid tlbl (EventTimerCancelled tmid)
             $ traceMany
                 [ (time, tid', tlbl', EventTxWakeup vids)
                 | tid' <- unblocked
                 , let tlbl' = lookupThreadLabel tid' threads
                 , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
             $ trace

    -- cancelling a negative timer is a no-op
    CancelTimeout (NegativeTimeout _tmid) k -> do
      -- negative timers are promptly removed from the state
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    Fork a k -> do
      let !nextId   = threadNextTId thread
          !tid'     = childThreadId tid nextId
          !thread'  = thread { threadControl = ThreadControl (k tid') ctl
                             , threadNextTId = succ nextId }
          !thread'' = Thread { threadId      = tid'
                             , threadControl = ThreadControl (runIOSim a)
                                                             ForkFrame
                             , threadStatus  = ThreadRunning
                             , threadMasking = threadMasking thread
                             , threadThrowTo = []
                             , threadClockId = threadClockId thread
                             , threadLabel   = Nothing
                             , threadNextTId = 1
                             }
          !threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = Deque.snoc tid' runqueue
                                         , threads  = threads' }
      return (SimTrace time tid tlbl (EventThreadForked tid') trace)

    Atomically a k ->
      execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x written _read created
                         tvarDynamicTraces tvarStringTraces nextVid' -> do
          (!wakeup, wokeby) <- threadsUnblockedByWrites written
          !_ <- mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads True wakeup simstate
          written' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
          created' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) created
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          !trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $ SimTrace time tid tlbl (EventTxCommitted
                                             written' created' Nothing)
                 $ traceMany
                     [ (time, tid', tlbl', EventTxWakeup vids')
                     | tid' <- unblocked
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
                 $ traceMany
                     [ (time, tid, tlbl, EventLog tr)
                     | tr <- tvarDynamicTraces ]
                 $ traceMany
                     [ (time, tid, tlbl, EventSay str)
                     | str <- tvarStringTraces ]
                 $ SimTrace time tid tlbl (EventUnblocked unblocked)
                 $ SimTrace time tid tlbl (EventDeschedule Yield)
                 $ trace

        StmTxAborted _read e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          !trace <- schedule thread' simstate
          return $ SimTrace time tid tlbl (EventTxAborted Nothing) trace

        StmTxBlocked read -> do
          !_ <- mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          !trace <- deschedule (Blocked BlockedOnSTM) thread simstate
          return $ SimTrace time tid tlbl (EventTxBlocked vids Nothing)
                 $ SimTrace time tid tlbl (EventDeschedule (Blocked BlockedOnSTM))
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
          Unmasked -> SimTrace time tid tlbl (EventDeschedule Interruptable)
                  <$> deschedule Interruptable thread' simstate
          _        -> schedule                 thread' simstate
      return $ SimTrace time tid tlbl (EventMask maskst')
             $ trace

    ThrowTo e tid' _ | tid' == tid -> do
      -- Throw to ourself is equivalent to a synchronous throw,
      -- and works irrespective of masking state since it does not block.
      let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventThrowTo e tid) trace)

    ThrowTo e tid' k -> do
      let thread'   = thread { threadControl = ThreadControl k ctl }
          willBlock = case Map.lookup tid' threads of
                        Just t -> not (threadInterruptible t)
                        _      -> False
      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t = t { threadThrowTo = (e, Labelled tid tlbl) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          !trace <- deschedule (Blocked BlockedOnThrowTo) thread' simstate { threads = threads' }
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ SimTrace time tid tlbl EventThrowToBlocked
                 $ SimTrace time tid tlbl (EventDeschedule (Blocked BlockedOnThrowTo))
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
          let adjustTarget t@Thread{ threadControl = ThreadControl _ ctl' } =
                t { threadControl = ThreadControl (Throw e) ctl'
                  , threadStatus  = ThreadRunning
                  }
              simstate'@SimState { threads = threads' }
                         = snd (unblockThreads False [tid'] simstate)
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          trace <- schedule thread' simstate''
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ trace

    YieldSim k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      deschedule Yield thread' simstate

    -- ExploreRaces is ignored by this simulator
    ExploreRaces k ->
      schedule thread{ threadControl = ThreadControl k ctl } simstate

    Fix f k -> do
      r <- newSTRef (throw NonTermination)
      x <- unsafeInterleaveST $ readSTRef r
      let k' = unIOSim (f x) $ \x' ->
                  LiftST (lazyToStrictST (writeSTRef r x')) (\() -> k x')
          thread' = thread { threadControl = ThreadControl k' ctl }
      schedule thread' simstate

    NewUnique k -> do
      let thread'   = thread{ threadControl = ThreadControl (k nextUniq) ctl }
          n         = unMkUnique nextUniq
          simstate' = simstate{ nextUniq = MkUnique (n + 1) }
      SimTrace time tid tlbl (EventUniqueCreated n)
        <$> schedule thread' simstate'


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                   -> True
      MaskedInterruptible
        | isThreadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise              -> False
      MaskedUninterruptible      -> False

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)
deschedule Yield !thread !simstate@SimState{runqueue, threads} =

    -- We don't interrupt runnable threads to provide fairness anywhere else.
    -- We do it here by putting the thread to the back of the runqueue, behind
    -- all other runnable threads.
    --
    -- For testing, we should have a more sophisticated policy to show that
    -- algorithms are not sensitive to the exact policy, so long as it is a
    -- fair policy (all runnable threads eventually run).

    let runqueue' = Deque.snoc (threadId thread) runqueue
        threads'  = Map.insert (threadId thread) thread threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }

deschedule Interruptable !thread@Thread {
                           threadId      = tid,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid') : etids,
                           threadLabel   = tlbl
                         }
                         !simstate@SimState{ curTime = time, threads } =

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids }
        (unblocked,
         simstate') = unblockThreads False [l_labelled tid'] simstate
    in do
    trace <- schedule thread' simstate'
    return $ SimTrace time tid tlbl (EventThrowToUnmasked tid')
           $ traceMany [ (time, tid'', tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
             trace

deschedule Interruptable !thread !simstate =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    schedule thread simstate

deschedule (Blocked _blockedReason) !thread@Thread { threadThrowTo = _ : _
                                                   , threadMasking = maskst } !simstate
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    deschedule Interruptable thread { threadMasking = Unmasked } simstate

deschedule (Blocked blockedReason) !thread !simstate@SimState{threads} =
    let thread'  = thread { threadStatus = ThreadBlocked blockedReason }
        threads' = Map.insert (threadId thread') thread' threads in
    reschedule simstate { threads = threads' }

deschedule Terminated !thread !simstate@SimState{ curTime = time, threads } =
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let !wakeup      = map (l_labelled . snd) (reverse (threadThrowTo thread))
        (unblocked,
         !simstate') = unblockThreads False wakeup simstate
    in do
    !trace <- reschedule simstate'
    return $ traceMany
               [ (time, tid', tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
               trace

deschedule Sleep _thread _simstate =
    error "IOSim: impossible happend"

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule :: SimState s a -> ST s (SimTrace a)
reschedule !simstate@SimState{ runqueue, threads }
  | Just (!tid, runqueue') <- Deque.uncons runqueue =
    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule !simstate@SimState{ threads, timers, curTime = time } =

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (TraceDeadlock time (labelledThreads threads))

      Just (tmids, !time', !fired, !timers') -> assert (time' >= time) $ do
        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        !written <- execAtomically' (runSTM $ mapM_ timeoutSTMAction fired)
        !ds  <- traverse (\(SomeTVar tvar) -> do
                            tr <- traceTVarST tvar False
                            !_ <- commitTVar tvar
                            return tr) written
        (wakeupSTM, wokeby) <- threadsUnblockedByWrites written
        !_ <- mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

            -- Check all fired threadDelays
        let wakeupThreadDelay = [ (tid, tmid) | TimerThreadDelay tid tmid <- fired ]
            !simstate'        =
                snd . unblockThreads False (fst `fmap` wakeupThreadDelay)
              . snd . unblockThreads True  wakeupSTM
              $ simstate

            -- For each 'timeout' action where the timeout has fired, start a
            -- new thread to execute throwTo to interrupt the action.
            !timeoutExpired = [ (tid, tmid, lock)
                              | TimerTimeout tid tmid lock <- fired ]

        !simstate'' <- forkTimeoutInterruptThreads timeoutExpired simstate'

        !trace <- reschedule simstate'' { curTime = time'
                                        , timers  = timers' }

        return $
          traceMany ([ ( time', ThreadId [-1], Just "timer"
                       , EventTimerFired tmid)
                     | (tmid, Timer _) <- zip tmids fired ]
                  ++ [ ( time', ThreadId [-1], Just "register delay timer"
                       , EventRegisterDelayFired tmid)
                     | (tmid, TimerRegisterDelay _) <- zip tmids fired ]
                  ++ [ (time', ThreadId [-1], Just "register delay timer", EventLog (toDyn a))
                     | TraceValue { traceDynamic = Just a } <- ds ]
                  ++ [ (time', ThreadId [-1], Just "register delay timer", EventSay a)
                     | TraceValue { traceString = Just a } <- ds ]
                  ++ [ (time', tid', tlbl', EventTxWakeup vids)
                     | tid' <- wakeupSTM
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
                  ++ [ ( time', tid, Just "thread delay timer"
                       , EventThreadDelayFired tmid)
                     | (tid, tmid) <- wakeupThreadDelay ]
                  ++ [ ( time', tid, Just "timeout timer"
                       , EventTimeoutFired tmid)
                     | (tid, tmid, _) <- timeoutExpired ]
                  ++ [ ( time', tid, Just "thread forked"
                       , EventThreadForked tid)
                     | (tid, _, _) <- timeoutExpired ])
                    trace
  where
    timeoutSTMAction :: TimerCompletionInfo s -> STM s ()
    timeoutSTMAction (Timer var) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()
    timeoutSTMAction (TimerRegisterDelay var) = writeTVar var True
    -- Note that 'threadDelay' is not handled via STM style wakeup, but rather
    -- it's handled directly above with 'wakeupThreadDelay' and 'unblockThreads'
    timeoutSTMAction TimerThreadDelay{}       = return ()
    timeoutSTMAction TimerTimeout{}           = return ()

unblockThreads :: Bool -> [IOSimThreadId] -> SimState s a -> ([IOSimThreadId], SimState s a)
unblockThreads !onlySTM !wakeup !simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadBlocked is correct)
    -- we update the runqueue and threads together here
    (unblocked, simstate {
                  runqueue = runqueue <> Deque.fromList unblocked,
                  threads  = threads'
                })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    !unblocked = [ tid
                 | tid <- wakeup
                 , case Map.lookup tid threads of
                    Just Thread { threadStatus = ThreadBlocked BlockedOnSTM }
                      -> True
                    Just Thread { threadStatus = ThreadBlocked _ }
                      -> not onlySTM
                    _ -> False
                 ]
    -- and in which case we mark them as now running
    !threads'  = List.foldl'
                   (flip (Map.adjust (\t -> t { threadStatus = ThreadRunning })))
                   threads
                   unblocked

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
              Nothing -> st { runqueue = Deque.snoc (threadId t) runqueue,
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
                    Nothing -> error ("IOSim: internal error: unknown thread " ++ show tid)
              nextId   = threadNextTId t
          in ( state { threads = Map.insert tid t { threadNextTId = succ nextId } threads }
             , ( Thread { threadId      = childThreadId tid nextId,
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
                            threadNextTId = 1
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
-- Also remove timeouts associated to frames we unwind.
--
unwindControlStack :: forall s a.
                      SomeException
                   -> Thread s a
                   -> Timeouts s
                   -> ( Either Bool (Thread s a)
                      , Timeouts s
                      )
unwindControlStack e thread = \timers ->
    case threadControl thread of
      ThreadControl _ ctl ->
        unwind (threadMasking thread) ctl timers
  where
    unwind :: forall s' c. MaskingState
           -> ControlStack s' c a
           -> IntPSQ Time (TimerCompletionInfo s)
           -> (Either Bool (Thread s' a), IntPSQ Time (TimerCompletionInfo s))
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
    unwind maskst (TimeoutFrame tmid _ k ctl) timers =
        case fromException e of
          -- Exception came from timeout expiring
          Just (TimeoutException tmid') | tmid == tmid' ->
            (Right thread { threadControl = ThreadControl (k Nothing) ctl }, timers')
          -- Exception came from a different exception
          _ -> unwind maskst ctl timers'
      where
        -- Remove the timeout associated with the 'TimeoutFrame'.
        timers' = (PSQ.delete . coerce) tmid timers

    unwind maskst (DelayFrame tmid _k ctl) timers =
        unwind maskst ctl timers'
      where
        -- Remove the timeout associated with the 'DelayFrame'.
        timers' = (PSQ.delete . coerce) tmid timers


    atLeastInterruptibleMask :: MaskingState -> MaskingState
    atLeastInterruptibleMask Unmasked = MaskedInterruptible
    atLeastInterruptibleMask ms       = ms


removeMinimums :: (Coercible k Int, Ord p)
               => IntPSQ p a
               -> Maybe ([k], p, [a], IntPSQ p a)
removeMinimums = \psq -> coerce $
    case PSQ.minView psq of
      Nothing              -> Nothing
      Just (k, p, x, psq') -> Just (collectAll [k] p [x] psq')
  where
    collectAll !ks !p !xs !psq =
      case PSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(Time, IOSimThreadId, Maybe ThreadLabel, SimEventType)]
          -> SimTrace a -> SimTrace a
traceMany []                      trace = trace
traceMany ((time, tid, tlbl, event):ts) trace =
    SimTrace time tid tlbl event (traceMany ts trace)

lookupThreadLabel :: IOSimThreadId -> Map IOSimThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in the lazy 'ST' monad.  One
-- can recover failures or the result from 'SimTrace' with
-- 'Control.Monad.IOSim.traceResult', or access 'SimEventType's generated by
-- the computation with 'Control.Monad.IOSim.traceEvents'.  A slightly more
-- convenient way is exposed by 'Control.Monad.IOSim.runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = schedule mainThread initialState
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
        threadNextTId = 1
      }


--
-- Executing STM Transactions
--

execAtomically :: forall s a c.
                  Time
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
                                []
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
          -- Skip the right hand alternative and continue with the k continuation
          go ctl' read written' writtenSeq' createdSeq' nextVid (k x)

      ThrowStm e -> do
        -- Rollback `TVar`s written since catch handler was installed
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
        -- Execute the catch handler with an empty written set.
        -- but preserve ones that were set prior to it, as specified in the
        -- [stm](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#v:catchSTM) package.
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
        go ctl read written writtenSeq (SomeTVar v : createdSeq) (succ nextVid) (k v)

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
        | otherwise ->
             do
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
        return $ SimTrace time tid tlbl (EventSay msg) trace

      OutputStm x k -> do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        return $ SimTrace time tid tlbl (EventLog x) trace

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
         == Set.fromList [ tvarId tvar | SomeTVar tvar <- writtenSeq ]


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
    !tvarLabel   <- newSTRef mbLabel
    !tvarCurrent <- newSTRef x
    !tvarUndo    <- newSTRef $! []
    !tvarBlocked <- newSTRef ([], Set.empty)
    !tvarVClock  <- newSTRef $! VectorClock Map.empty
    !tvarTrace   <- newSTRef $! Nothing
    return TVar {tvarId, tvarLabel, tvarCurrent, tvarUndo, tvarBlocked,
                 tvarVClock, tvarTrace}


-- 'execReadTVar' is defined in `Control.Monad.IOSim.Type` and shared with /IOSimPOR/

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
traceTVarST TVar{tvarId, tvarCurrent, tvarUndo, tvarTrace} new = do
    mf <- readSTRef tvarTrace
    case mf of
      Nothing -> return TraceValue { traceDynamic = (Nothing :: Maybe ())
                                   , traceString = Nothing }
      Just f  -> do
        !vs <- readSTRef tvarUndo
        v   <- readSTRef tvarCurrent
        case (new, vs) of
          (True, _) -> f Nothing v
          (_, _:_)  -> f (Just $ last vs) v
          _         -> error ("traceTVarST: unexpected tvar state " ++ show tvarId)



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
      !_ <- writeSTRef tvarBlocked (tids', tidsSet')
      return ()

unblockAllThreadsFromTVar :: TVar s a -> ST s ()
unblockAllThreadsFromTVar TVar{tvarBlocked} = do
    !_ <- writeSTRef tvarBlocked ([], Set.empty)
    return ()

-- | For each TVar written to in a transaction (in order) collect the threads
-- that blocked on each one (in order).
--
-- Also, for logging purposes, return an association between the threads and
-- the var writes that woke them.
--
threadsUnblockedByWrites :: [SomeTVar s]
                         -> ST s ([IOSimThreadId], Map IOSimThreadId (Set (Labelled TVarId)))
threadsUnblockedByWrites written = do
  !tidss <- sequence
             [ (,) <$> labelledTVarId tvar <*> readTVarBlockedThreads tvar
             | SomeTVar tvar <- written ]
  -- Threads to wake up, in wake up order, annotated with the vars written that
  -- caused the unblocking.
  -- We reverse the individual lists because the tvarBlocked is used as a stack
  -- so it is in order of last written, LIFO, and we want FIFO behaviour.
  let !wakeup = ordNub [ tid | (_vid, tids) <- tidss, tid <- reverse tids ]
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
{-# INLINE ordNub #-}
