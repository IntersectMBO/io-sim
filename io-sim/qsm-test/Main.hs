module Main where

import BasicTest.Correct qualified
import BasicTest.InternalFork qualified
import BasicTest.InternalForkAtomic qualified
import BasicTest.NoIncr qualified
import BasicTest.Racy qualified
import Exception.Deadlocks qualified
import Exception.TryPutMVar qualified
import Exception.WithMVar qualified
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $
  testGroup "QSM" [
    testGroup "Sequential" [
        -- Fully correct implementation
        testProperty "Correct"                       BasicTest.Correct.prop_sequential
        -- Buggy implementation, doesn't increment. The error is found in sequential testing
      , testProperty "NoIncr"        $ expectFailure BasicTest.NoIncr.prop_sequential
        -- Racy implementation, sequential testing passes
      , testProperty "Racy"                          BasicTest.Racy.prop_sequential
      , testProperty "Internal"                      BasicTest.InternalFork.prop_sequential
      , testProperty "Internal'"                     BasicTest.InternalForkAtomic.prop_sequential
      ]
    , testGroup "SequentialPOR" [
        testProperty "Internal"      $ expectFailure BasicTest.InternalFork.prop_sequential'
        -- TODO this is still too slow because it will replay all possible reorderings from the past
        -- @exploreRaces@ should be called at an appropriate point in time?
        -- , testProperty "Internal'" $                BasicTest.InternalForkAtomic.prop_sequential'
      ]
    , testGroup "Parallel" [
        -- Fully correct implementation
        testProperty "Correct"                       BasicTest.Correct.prop_parallel
        -- Racy implementation, parallel test case fails
      , testProperty "Racy"          $ expectFailure BasicTest.Racy.prop_parallel
      ]
    , testGroup "Exceptions" [
        testGroup "DoublePut" [
          testProperty "Sequential"                    Exception.Deadlocks.prop_sequential
        , testProperty "SequentialPOR" $ expectFailure Exception.Deadlocks.prop_sequential'
        -- , testProperty "Parallel"                      Exception.Deadlocks.prop_parallel
        ]
      , testGroup "TryPutMVar" [
          testProperty "Parallel"      $ expectFailure Exception.TryPutMVar.prop_parallel'
        ]
      , testGroup "WithMVar" [
          testProperty "Parallel"                      Exception.WithMVar.prop_parallel'
        ]
      ]
  ]
