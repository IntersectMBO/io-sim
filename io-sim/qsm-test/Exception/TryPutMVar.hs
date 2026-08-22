{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |

module Exception.TryPutMVar where

import Data.Kind
import GHC.Generics
import Test.QuickCheck
import Test.StateMachine.IOSim
import Test.StateMachine.Types.Rank2 qualified as Rank2
import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadThrow

{-------------------------------------------------------------------------------
  Cmd and response
-------------------------------------------------------------------------------}

type Cmd :: (Type -> Type) -> Type
data Cmd r = Increment
  deriving (Eq, Show, Generic, Generic1, Read)

deriving instance Rank2.Functor Cmd
deriving instance Rank2.Traversable Cmd
deriving instance Rank2.Foldable Cmd

type Resp :: (Type -> Type) -> Type
data Resp r = Void
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
postcondition _ _ _                   = Top

invariant :: Maybe (model Concrete -> Logic)
invariant = Nothing

generator :: model Symbolic -> Maybe (Gen (Cmd Symbolic))
generator _ = Just $ pure Increment

shrinker :: model Symbolic -> cmd Symbolic -> [cmd Symbolic]
shrinker _ _ = []

mock :: Model Symbolic -> Cmd Symbolic -> GenSym (Resp Symbolic)
mock _ Increment = pure Void

{-------------------------------------------------------------------------------
  SUT
-------------------------------------------------------------------------------}

newtype AtomicCounter m = AtomicCounter (MVar m Int)

newSUT :: MonadLabelledMVar m => m (AtomicCounter m)
newSUT = do
    ref <- newMVar 0
    labelMVar ref "TheCounter"
    return (AtomicCounter ref)

incr :: (MonadCatch m, MonadMVar m) => AtomicCounter m -> m ()
incr (AtomicCounter ref) =
  bracketOnError
    (takeMVar ref)
    (tryPutMVar ref)
    (putMVar ref . (+1))

semantics :: (MonadCatch m, MonadMVar m) => AtomicCounter m -> Cmd Concrete -> m (Resp Concrete)
semantics sut Increment = do
  incr sut *> pure Void

transition :: Model r -> Cmd r -> Resp r -> Model r
transition (Model m) Increment _ = Model (m + 1)

sm :: StateMachine Model Cmd AtomicCounter Resp
sm = StateMachine initModel transition precondition postcondition Nothing
     generator shrinker newSUT semantics mock

prop_sequential :: Property
prop_sequential = forAllCommands sm Nothing $ runSequential sm

prop_sequential' :: Property
prop_sequential' = forAllCommands sm Nothing $ runSequentialPORException sm

prop_parallel :: Property
prop_parallel = forAllParallelCommands sm Nothing $ runParallel sm

prop_parallel' :: Property
prop_parallel' = forAllParallelCommands sm Nothing $ runParallelException sm
