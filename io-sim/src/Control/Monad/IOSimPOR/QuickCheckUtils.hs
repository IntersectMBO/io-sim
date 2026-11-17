{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.IOSimPOR.QuickCheckUtils where

import Control.Exception (evaluate)
import Control.Parallel
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck.Property

-- Take the conjunction of several properties, in parallel.  This is a
-- modification of code from Test.QuickCheck.Property.
--
-- All conjuncts but the first one are sparked; the first one is reduced by
-- the thread which reduces the conjunction.  An 'IORose' is reduced with
-- 'unsafePerformIO': the properties built by 'exploreSimTrace' are always
-- 'IORose's, since QuickCheck's combinators ('counterexample', 'tabulate',
-- ...) wrap them in exception handlers, but the IO they do is evaluation.
--
-- The sparks are created by the 'IORose' action of the conjunction, which
-- runs once per reduction of the conjunction.  Had they been created while
-- evaluating a lazy list, two threads could evaluate the same list cell
-- (lazy black-holing), and the thread which loses that race would leave
-- behind sparks which reduce the remaining conjuncts a second time.
-- 'unsafePerformIO' (rather than 'unsafeDupablePerformIO') ensures that the
-- thread which consumes the conjunction and a spark do not both reduce the
-- same conjunct.
conjoinPar :: Testable prop => [prop] -> Property
conjoinPar ps =
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . property) ps
     return $ MkProp $ IORose $ do
       let roses' = map speculate roses
       mapM_ (\rose -> evaluate (rose `par` ())) (drop 1 roses')
       return (conjRoses roses')
  where
    speculate rose = case rose of
      MkRose result _ -> forceResult result `pseq` rose
      IORose _        ->
        case unsafePerformIO (reduceRose rose) of
          rose'@(MkRose result _) -> forceResult result `pseq` rose'
          rose'                   -> rose'

    forceResult result = case ok result of { Nothing -> (); Just !_ -> () }

-- | Conjunction of rose trees.  It runs the 'IO' of each 'IORose' at most
-- once.
conjRoses :: [Rose Result] -> Rose Result
conjRoses = conj id
  where
  conj :: (Result -> Result) -> [Rose Result] -> Rose Result
  conj k [] =
    MkRose (k succeeded) []

  conj k (p : ps) = do
    result <- p
    case ok result of
      _ | not (expect result) ->
        return failed { reason = "expectFailure may not occur inside a conjunction" }
      Just True -> conj (addLabels result . addCallbacksAndCoverage result . k) ps
      Just False -> return result
      Nothing -> do
        let rest = conj (addCallbacksAndCoverage result . k) ps
        result2 <- rest
        -- Nasty work to make sure we use the right callbacks
        case ok result2 of
          Just True  -> MkRose (result2 { ok = Nothing }) []
          Just False -> return result2
          Nothing    -> return result2

  addCallbacksAndCoverage result r =
    r { callbacks = callbacks result ++ callbacks r,
        requiredCoverage = requiredCoverage result ++ requiredCoverage r }
  addLabels result r =
    r { labels = labels result ++ labels r,
        classes = classes result ++ classes r,
        tables = tables result ++ tables r }
