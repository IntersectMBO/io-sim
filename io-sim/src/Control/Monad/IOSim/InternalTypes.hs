{-# LANGUAGE GADTs #-}

-- | Internal types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.InternalTypes
  ( ThreadControl (..)
  , ControlStack (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Class.MonadThrow (MaskingState (..))

import Control.Monad.IOSim.Types (SimA (..), TimeoutId)

-- We hide the type @b@ here, so it's useful to bundle these two parts together,
-- rather than having Thread have an existential type, which makes record
-- updates awkward.
data ThreadControl s a where
  ThreadControl :: SimA s b
                -> !(ControlStack s b a)
                -> ThreadControl s a

instance Show (ThreadControl s a) where
  show _ = "..."

data ControlStack s b a where
  MainFrame  :: ControlStack s a  a
  ForkFrame  :: ControlStack s () a
  MaskFrame  :: (b -> SimA s c)         -- subsequent continuation
             -> MaskingState            -- thread local state to restore
             -> !(ControlStack s c a)
             -> ControlStack s b a
  CatchFrame :: Exception e
             => (e -> SimA s b)         -- exception continuation
             -> (b -> SimA s c)         -- subsequent continuation
             -> !(ControlStack s c a)
             -> ControlStack s b a
  TimeoutFrame :: TimeoutId
               -> (Maybe b -> SimA s c)
               -> !(ControlStack s c a)
               -> ControlStack s b a
  DelayFrame   :: TimeoutId
               -> SimA s c
               -> ControlStack s c a
               -> ControlStack s b a

instance Show (ControlStack s b a) where
  show = show . dash
    where
      dash :: ControlStack s b' a -> ControlStackDash
      dash MainFrame                  = MainFrame'
      dash ForkFrame                  = ForkFrame'
      dash (MaskFrame _ m cs)         = MaskFrame' m (dash cs)
      dash (CatchFrame _ _ cs)        = CatchFrame' (dash cs)
      dash (TimeoutFrame tmid _ cs)   = TimeoutFrame' tmid (dash cs)
      dash (DelayFrame tmid _ cs)     = DelayFrame' tmid (dash cs)

data ControlStackDash =
    MainFrame'
  | ForkFrame'
  | MaskFrame' MaskingState ControlStackDash
  | CatchFrame' ControlStackDash
  | TimeoutFrame' TimeoutId ControlStackDash
  | ThreadDelayFrame' TimeoutId ControlStackDash
  | DelayFrame' TimeoutId ControlStackDash
  deriving Show
