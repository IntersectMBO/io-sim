{-# LANGUAGE RankNTypes #-}
-- |

module Test.StateMachine.IOSim.Types (StateMachine (..)) where

import Control.Monad.IOSim
import Data.Functor.Classes
import Test.QuickCheck
import Test.StateMachine.Logic
import Test.StateMachine.Types hiding (StateMachine (..))

data StateMachine model cmd sut resp = StateMachine
    { initModel :: forall r. model r
    , transition :: forall r. (Show1 r, Ord1 r) => model r -> cmd r -> resp r -> model r
    , precondition :: model Symbolic -> cmd Symbolic -> Logic
    , postcondition :: model Concrete -> cmd Concrete -> resp Concrete -> Logic
    , invariant :: Maybe (model Concrete -> Logic)
    , generator :: model Symbolic -> Maybe (Gen (cmd Symbolic))
    , shrinker :: model Symbolic -> cmd Symbolic -> [cmd Symbolic]
    , initSut :: forall s. IOSim s (sut (IOSim s))
    , semantics :: forall s. sut (IOSim s) -> cmd Concrete -> IOSim s (resp Concrete)
    , mock :: model Symbolic -> cmd Symbolic -> GenSym (resp Symbolic)
    }
