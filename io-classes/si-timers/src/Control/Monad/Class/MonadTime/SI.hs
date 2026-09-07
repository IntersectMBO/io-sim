{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Monad.Class.MonadTime.SI
  ( MonadTime (..)
  , MonadMonotonicTime (..)
  , TimeDuration (..)
    -- * 'DiffTime' and its action on 'Time'
  , Time (..)
  , DiffTime
    -- * 'NominalTime' and its action on 'UTCTime'
  , UTCTime
  , diffUTCTime
  , addUTCTime
  , NominalDiffTime
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.Reader

import Control.Monad.Class.MonadTime (MonadMonotonicTimeNSec, MonadTime (..),
           NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Control.Monad.Class.MonadTime qualified as MonadTime

import NoThunks.Class (NoThunks (..))

import Data.Fixed (Pico, showFixed)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock qualified as Time
import Data.Word (Word64)
import GHC.Generics (Generic (..))


-- | A point in time in a monotonic clock counted in seconds.
--
-- The epoch for this clock is arbitrary and does not correspond to any wall
-- clock or calendar, and is /not guaranteed/ to be the same epoch across
-- program runs. It is represented as the 'DiffTime' from this arbitrary epoch.
--
newtype Time = Time DiffTime
  deriving stock    (Eq, Ord, Generic)
  deriving newtype  NFData
  deriving anyclass NoThunks

instance Show Time where
  show (Time t) = "Time " ++ showFixed True (realToFrac t :: Pico)

-- | Time type @t@ paired with its associated duration type
-- @'Duration' t@ forming a monoid action.
--
class TimeDuration t where
  -- | The time duration between two points in time (positive or negative).
  type Duration t = d | d -> t

  -- | @'diffT' t2 t1@ is the duration from @t1@ to @t2@.
  diffTime :: t -> t -> Duration t

  -- | Add a duration to a point in time, giving another time.
  addTime  :: Duration t -> t -> t

infixr 9 `addTime`

instance TimeDuration UTCTime where
  type Duration UTCTime = NominalDiffTime
  diffTime = diffUTCTime
  addTime = addUTCTime

instance TimeDuration Time where
  type Duration Time = DiffTime
  diffTime (Time t) (Time t') = t - t'
  addTime d (Time t) = Time (d + t)


class MonadMonotonicTimeNSec m => MonadMonotonicTime m where
  getMonotonicTime :: m Time

  default getMonotonicTime :: m Time
  getMonotonicTime =
        conv <$> MonadTime.getMonotonicTimeNSec
      where
        conv :: Word64 -> Time
        conv = Time . Time.picosecondsToDiffTime . (* 1_000) . toInteger

instance MonadMonotonicTime IO where

--
-- MTL instances
--

instance MonadMonotonicTime m => MonadMonotonicTime (ReaderT r m) where
  getMonotonicTime = lift getMonotonicTime
