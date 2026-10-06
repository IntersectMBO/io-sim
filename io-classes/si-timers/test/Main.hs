module Main (main) where

import Test.Tasty

import Test.MonadTimer qualified
import Test.MonadTimer.IO qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "io-classes"
    [ Test.MonadTimer.tests
    , Test.MonadTimer.IO.tests
    ]

