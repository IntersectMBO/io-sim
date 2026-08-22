{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |

module BasicTest.Template where

import Data.Kind
import GHC.Generics
import Test.QuickCheck
import Test.StateMachine.IOSim
import Test.StateMachine.Types.Rank2 qualified as Rank2

{-------------------------------------------------------------------------------
  Cmd and response
-------------------------------------------------------------------------------}

type Cmd :: (Type -> Type) -> Type
data Cmd r = Incr | Get
  deriving (Eq, Show, Generic, Generic1, Read)

deriving instance Rank2.Functor Cmd
deriving instance Rank2.Traversable Cmd
deriving instance Rank2.Foldable Cmd

type Resp :: (Type -> Type) -> Type
data Resp r = Void | GetR Int
  deriving (Eq, Show, Generic, Generic1, Read)

deriving instance Rank2.Functor Resp
deriving instance Rank2.Traversable Resp
deriving instance Rank2.Foldable Resp

{-------------------------------------------------------------------------------
  Model
--------------------------------------------------------------------------------}

type Model :: (Type -> Type) -> Type
data Model r = Model
 { value :: Int }
 deriving (Show)

initModel :: Model r
initModel = Model 0

precondition :: model Symbolic -> cmd Symbolic -> Logic
precondition _ _ = Top

postcondition :: Model Concrete -> Cmd Concrete -> Resp Concrete -> Logic
postcondition (Model m) Get (GetR m') = m .== m'
postcondition _ _ _                   = Top

invariant :: Maybe (model Concrete -> Logic)
invariant = Nothing

generator :: model Symbolic -> Maybe (Gen (Cmd Symbolic))
generator _ = Just $ oneof [pure Incr, pure Get]

shrinker :: model Symbolic -> cmd Symbolic -> [cmd Symbolic]
shrinker _ _ = []

mock :: Model Symbolic -> Cmd Symbolic -> GenSym (Resp Symbolic)
mock _ Incr        = pure Void
mock (Model m) Get = pure (GetR m)
