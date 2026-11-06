{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE RoleAnnotations    #-}

-- | Common types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.CommonTypes
  ( IOSimThreadId (..)
  , ppIOSimThreadId
  , StepId
  , ppStepId
  , childThreadId
  , TVarId (..)
  , VarId
  , TimeoutId (..)
  , ClockId (..)
  , VectorClock (..)
  , ppVectorClock
  , unTimeoutId
  , ThreadLabel
  , TVarLabel
  , TVar (..)
  , SomeTVar (..)
  , someTVarToLabelled
  , Deschedule (..)
  , ThreadStatus (..)
  , BlockedReason (..)
  , Labelled (..)
  , ppLabelled
  , Unique (..)
    -- * Utils
  , ppList
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.Class.MonadSTM (TraceValue)
import Control.Monad.ST.Lazy

import NoThunks.Class

import Data.Hashable
import Data.List (intercalate, intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.STRef.Lazy
import GHC.Generics
import Quiet


-- | A thread id.
--
-- /IOSimPOR/: 'RacyThreadId' indicates that this thread is taken into account
-- when discovering races.  A thread is marked as racy iff
-- `Control.Monad.Class.MonadTest.exploreRaces` was
-- executed in it or it's a thread forked by a racy thread.
--
data IOSimThreadId =
    -- | A racy thread (`IOSimPOR` only), shown in the trace with curly braces,
    -- e.g. `Thread {2,3}`.
    RacyThreadId [Int]
    -- | A non racy thread.  They have higher priority than racy threads in
    -- `IOSimPOR` scheduler.
  | ThreadId     [Int]
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving anyclass NoThunks

instance Hashable IOSimThreadId

ppIOSimThreadId :: IOSimThreadId -> String
ppIOSimThreadId (RacyThreadId as) = "Thread {"++ intercalate "," (map show as) ++"}"
ppIOSimThreadId     (ThreadId as) = "Thread " ++ show as

childThreadId :: Bool -> IOSimThreadId -> Int -> IOSimThreadId
childThreadId _     (RacyThreadId is) i = RacyThreadId (is ++ [i])
childThreadId False (ThreadId     is) i = ThreadId     (is ++ [i])
childThreadId True  (ThreadId     is) i = RacyThreadId (is ++ [i])

-- | Execution step in `IOSimPOR` is identified by the thread id and
-- a monotonically increasing number (thread specific).
--
type StepId = (IOSimThreadId, Int)

ppStepId :: (IOSimThreadId, Int) -> String
ppStepId (tid, step) | step < 0
                     = concat [ppIOSimThreadId tid, ".-"]
ppStepId (tid, step) = concat [ppIOSimThreadId tid, ".", show step]


type VarId = Int
-- | 'TVar's are used to emulate other shared variables. Each one comes with
-- its own id constructor.
data TVarId =
    TVarId  !VarId
    -- ^ a `TVar`
  | TMVarId !VarId
    -- ^ a `TMVar` simulated by a `TVar`.
  | MVarId  !VarId
    -- ^ an `MVar` simulated by a `TVar`.
  | TQueueId !VarId
    -- ^ a 'TQueue` simulated by a `TVar`.
  | TBQueueId !VarId
    -- ^ a 'TBQueue` simulated by a `TVar`.
  | TSemId !VarId
    -- ^ a 'TSem` simulated by a `TVar`.
  -- TODO: `TChan`
  deriving (Eq, Ord, Show)
newtype TimeoutId   = TimeoutId Int   deriving (Eq, Ord, Enum, Show)
newtype ClockId     = ClockId   [Int] deriving (Eq, Ord, Show)
newtype VectorClock = VectorClock { getVectorClock :: Map IOSimThreadId Int }
  deriving Generic
  deriving Show via Quiet VectorClock

ppVectorClock :: VectorClock -> String
ppVectorClock (VectorClock m) = "VectorClock " ++ "[" ++ concat (intersperse ", " (ppStepId <$> Map.toList m)) ++ "]"

unTimeoutId :: TimeoutId -> Int
unTimeoutId (TimeoutId a) = a

type ThreadLabel = String
type TVarLabel   = String

data TVar s a = TVar {

       -- | The identifier of this var.
       --
       tvarId      :: !TVarId,

       -- | Label.
       tvarLabel   :: !(STRef s (Maybe TVarLabel)),

       -- | The var's current value
       --
       tvarCurrent :: !(STRef s a),

       -- | A stack of undo values. This is only used while executing a
       -- transaction.
       --
       tvarUndo    :: !(STRef s [a]),

       -- | Thread Ids of threads blocked on a read of this var. It is
       -- represented in reverse order of thread wakeup, without duplicates.
       --
       -- To avoid duplicates efficiently, the operations rely on a copy of the
       -- thread Ids represented as a set.
       --
       tvarBlocked :: !(STRef s ([IOSimThreadId], Set IOSimThreadId)),

       -- | The vector clock of the current value.
       --
       tvarVClock  :: !(STRef s VectorClock),

       -- | Callback to construct a trace which will be attached to the dynamic
       -- trace each time the `TVar` is committed.
       tvarTrace   :: !(STRef s (Maybe (Maybe a -> a -> ST s TraceValue)))
     }

instance Eq (TVar s a) where
    TVar {tvarId = a} == TVar {tvarId = b} = a == b

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

someTVarToLabelled :: SomeTVar s -> ST s (Labelled (SomeTVar s))
someTVarToLabelled tv@(SomeTVar var) = do
  lbl <- readSTRef (tvarLabel var)
  pure (Labelled tv lbl)

data Deschedule = Yield
                | Interruptable
                | Blocked BlockedReason
                | Terminated
                | Sleep
  deriving Show

data ThreadStatus = ThreadRunning
                  | ThreadBlocked BlockedReason
                  | ThreadDone
  deriving (Eq, Show)

data BlockedReason = BlockedOnSTM
                   | BlockedOnDelay
                   | BlockedOnThrowTo
  deriving (Eq, Show)

-- | A labelled value.
--
-- For example 'labelThread' or `labelTVar' will insert a label to `IOSimThreadId`
-- (or `TVarId`).
data Labelled a = Labelled {
    l_labelled :: !a,
    l_label    :: !(Maybe String)
  }
  deriving (Eq, Ord, Generic, Functor)
  deriving Show via Quiet (Labelled a)

ppLabelled :: (a -> String) -> Labelled a -> String
ppLabelled pp Labelled { l_labelled = a, l_label = Nothing  } = pp a
ppLabelled pp Labelled { l_labelled = a, l_label = Just lbl } = concat ["Labelled ", pp a, " ", lbl]

-- | Abstract unique symbols à la "Data.Unique".
newtype Unique s = MkUnique{ unMkUnique :: Integer }
  deriving stock   (Eq, Ord)
  deriving newtype NFData
type role Unique nominal

instance Hashable (Unique s) where
  hash = fromInteger . unMkUnique
  hashWithSalt = defaultHashWithSalt

--
-- Utils
--

ppList :: (a -> String) -> [a] -> String
ppList pp as = "[" ++ concat (intersperse ", " (map pp as)) ++ "]"
