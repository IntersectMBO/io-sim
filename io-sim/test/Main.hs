module Main (main) where

import Test.Tasty

import Test.Control.Concurrent.Class.MonadMVar qualified (tests)
import Test.Control.Concurrent.Class.MonadMVar.Strict qualified (tests)
import Test.Control.Monad.IOSim qualified (tests)
import Test.Control.Monad.IOSim.Preemption qualified (tests)
import Test.Control.Monad.IOSimPOR qualified (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "io-sim-tests"
  [ Test.Control.Concurrent.Class.MonadMVar.tests
  , Test.Control.Concurrent.Class.MonadMVar.Strict.tests
  , Test.Control.Monad.IOSim.tests
  , Test.Control.Monad.IOSim.Preemption.tests
  , Test.Control.Monad.IOSimPOR.tests
  ]
