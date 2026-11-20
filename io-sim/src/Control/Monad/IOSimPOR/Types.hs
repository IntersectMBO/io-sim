module Control.Monad.IOSimPOR.Types
  ( -- * Effects
    Effect (..)
  , readEffects
  , writeEffects
  , forkEffect
  , throwToEffect
  , wakeupEffects
  , onlyReadEffect
  , racingEffects
  , ppEffect
    -- * Schedules
  , ScheduleControl (..)
  , isDefaultSchedule
  , ScheduleMod (..)
    -- * Steps
  , StepId
  , ppStepId
  , Step (..)
  , StepInfo (..)
    -- * Races
  , Races (..)
  , noRaces
  ) where

import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Monad.IOSim.CommonTypes

--
-- Effects
--

-- | An `Effect` aggregates effects performed by a thread in a given
-- execution step.  Only used by *IOSimPOR*.
--
data Effect = Effect {
    effectReads  :: !(Set (Labelled TVarId)),
    effectWrites :: !(Set (Labelled TVarId)),
    effectForks  :: !(Set IOSimThreadId),
    effectThrows :: ![IOSimThreadId],
    effectWakeup :: !(Set IOSimThreadId)
  }
  deriving (Show, Eq)

ppEffect :: Effect -> String
ppEffect Effect { effectReads, effectWrites, effectForks, effectThrows, effectWakeup } =
  "Effect { " ++
    concat (List.intersperse ", " $
           [ "reads = "  ++ ppList (ppLabelled show) (Set.toList effectReads)  | not (null effectReads)  ]
        ++ [ "writes = " ++ ppList (ppLabelled show) (Set.toList effectWrites) | not (null effectWrites) ]
        ++ [ "forks = "  ++ ppList ppIOSimThreadId (Set.toList effectForks)    | not (null effectForks)  ]
        ++ [ "throws = " ++ ppList ppIOSimThreadId             effectThrows    | not (null effectThrows) ]
        ++ [ "wakeup = " ++ ppList ppIOSimThreadId (Set.toList effectWakeup)   | not (null effectWakeup) ])
        ++ " }"


instance Semigroup Effect where
  Effect r w s ts wu <> Effect r' w' s' ts' wu' =
    Effect (r <> r') (w <> w') (s <> s') (ts ++ ts') (wu <> wu')

instance Monoid Effect where
  mempty = Effect Set.empty Set.empty Set.empty [] Set.empty

--
-- Effect smart constructors
--

readEffects :: [Labelled (SomeTVar s)] -> Effect
readEffects rs = mempty{effectReads = Set.fromList (map (someTvarId <$>) rs)}

writeEffects :: [Labelled (SomeTVar s)] -> Effect
writeEffects rs = mempty{effectWrites = Set.fromList (map (someTvarId <$>) rs)}

forkEffect :: IOSimThreadId -> Effect
forkEffect tid = mempty{effectForks = Set.singleton tid}

throwToEffect :: IOSimThreadId -> Effect
throwToEffect tid = mempty{ effectThrows = [tid] }

wakeupEffects :: [IOSimThreadId] -> Effect
wakeupEffects tids = mempty{effectWakeup = Set.fromList tids}

--
-- Utils
--

someTvarId :: SomeTVar s -> TVarId
someTvarId (SomeTVar r) = tvarId r

onlyReadEffect :: Effect -> Bool
onlyReadEffect e = e { effectReads = effectReads mempty } == mempty

-- | Check if two effects race.  The two effects are assumed to come from
-- different threads, from steps which do not wake one another, see
-- `racingSteps`.
--
racingEffects :: Effect -> Effect -> Bool
racingEffects e e' =

       -- both effects throw to the same threads
       effectThrows e `intersectsL` effectThrows e'

       -- concurrent reads & writes of the same TVars
    || effectReads  e `intersects`  effectWrites e'
    || effectWrites e `intersects`  effectReads  e'

       -- concurrent writes to the same TVars
    || effectWrites e `intersects`  effectWrites e'

       -- one effect wakes up a thread which the other one throws to: the
       -- order decides if the exception finds the thread blocked, hence
       -- interruptible even if it is masked
    || effectWakeup e  `intersectsSL` effectThrows e'
    || effectWakeup e' `intersectsSL` effectThrows e

  where
    intersects :: Ord a => Set a -> Set a -> Bool
    intersects a b = not $ a `Set.disjoint` b

    intersectsL :: Eq a => [a] -> [a] -> Bool
    intersectsL a b = not $ null $ a `List.intersect` b

    intersectsSL :: Ord a => Set a -> [a] -> Bool
    intersectsSL a b = any (`Set.member` a) b


---
--- Schedules
---

-- | Modified execution schedule.
--
data ScheduleControl = ControlDefault
                     -- ^ default scheduling mode
                     | ControlAwait [ScheduleMod]
                     -- ^ if the current control is 'ControlAwait', the normal
                     -- scheduling will proceed, until the thread found in the
                     -- first 'ScheduleMod' reaches the given step.  At this
                     -- point the thread is put to sleep, until after all the
                     -- steps are followed.
                     | ControlFollow [StepId] [ScheduleMod]
                     -- ^ follow the steps then continue with schedule
                     -- modifications.  This control is set by 'followControl'
                     -- when 'controlTargets' returns true.
  deriving (Eq, Ord, Show)


isDefaultSchedule :: ScheduleControl -> Bool
isDefaultSchedule ControlDefault        = True
isDefaultSchedule (ControlFollow [] []) = True
isDefaultSchedule _                     = False

-- | A schedule modification inserted at given execution step.
--
data ScheduleMod = ScheduleMod{
    -- | Step at which the 'ScheduleMod' is activated.
    scheduleModTarget    :: StepId,
    -- | 'ScheduleControl' at the activation step.  It is needed by
    -- 'extendScheduleControl' when combining the discovered schedule with the
    -- initial one.
    scheduleModControl   :: ScheduleControl,
    -- | Series of steps which are executed at the target step.  This *includes*
    -- the target step, not necessarily as the last step.
    scheduleModInsertion :: [StepId]
  }
  deriving (Eq, Ord)


instance Show ScheduleMod where
  showsPrec d (ScheduleMod tgt ctrl insertion) =
    showParen (d>10) $
      showString "ScheduleMod " .
      showParen True (showString (ppStepId tgt)) .
      showString " " .
      showsPrec 11 ctrl .
      showString " [" .
      showString (List.intercalate "," (map ppStepId insertion)) .
      showString "]"

--
-- Steps
--

-- | A unit of execution.  `deschedule` marks a boundary of a `Step`, see it's
-- haddocks.
--
data Step = Step {
    stepThreadId :: !IOSimThreadId,
    stepStep     :: !Int,
    stepEffect   :: !Effect,
    stepVClock   :: !VectorClock
  }
  deriving Show


--
-- StepInfo
--

-- | As we execute a simulation, we collect information about each step.  This
-- information is updated as the simulation evolves by
-- `Control.Monad.IOSimPOR.Types.updateRaces`.
--
data StepInfo = StepInfo {
    -- | Step that we want to reschedule to run after a step in `stepInfoRaces`
    -- (there will be one schedule modification per step in
    -- `stepInfoRaces`).
    stepInfoStep       :: !Step,

    -- | Control information when we reached this step.
    stepInfoControl    :: !ScheduleControl,

    -- | Threads that are still concurrent with this step.
    stepInfoConcurrent :: !(Set IOSimThreadId),

    -- | Steps following this one that did not happen after it
    -- (in reverse order).
    stepInfoNonDep     :: ![Step],

    -- | Later steps that race with `stepInfoStep`.
    stepInfoRaces      :: ![Step]
  }
  deriving Show

--
-- Races
--

-- | Information about all discovered races in a simulation categorised as
-- active and complete races.
--
-- See 'normalizeRaces' how we split `StepInfo` into the two categories.
--
data Races = Races {
    -- | These steps may still race with future steps.
    activeRaces   :: ![StepInfo],

    -- | These steps cannot be concurrent with future steps.
    completeRaces :: ![StepInfo]
  }
  deriving Show

noRaces :: Races
noRaces = Races [] []
