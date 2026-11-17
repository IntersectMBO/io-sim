{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.IOSimPOR.QuickCheckUtils where

import Control.Exception (evaluate)
import Control.Parallel
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck.Gen
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
conjoinPar :: TestableNoCatch prop => [prop] -> Property
conjoinPar ps =
  againNoCatch $
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . propertyNoCatch) ps
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

-- We also need a version of conjoin that is sequential, but does not
-- label its result as an IO property unless one of its arguments
-- is. Consequently it does not catch exceptions in its arguments.
conjoinNoCatch :: TestableNoCatch prop => [prop] -> Property
conjoinNoCatch ps =
  againNoCatch $
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . propertyNoCatch) ps
     return (MkProp $ conjRoses roses)

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


-- .&&| is a sequential, but parallelism-friendly version of .&&., that
-- tests its arguments in sequence, but does not label its result as
-- an IO property unless one of its arguments is.

infixr 1 .&&|
(.&&|) :: TestableNoCatch prop => prop -> prop -> Property
p .&&| q = conjoinNoCatch [p, q]


-- property catches exceptions in its argument, turning everything
-- Testable into an IORose property, which cannot be paralellized. We
-- need an alternative that permits parallelism by allowing exceptions
-- to propagate. This is a modified clone of code from
-- Test.QuickCheck.Property.

class TestableNoCatch prop where
  propertyNoCatch :: prop -> Property

instance TestableNoCatch Discard where
  propertyNoCatch _ = propertyNoCatch rejected

instance TestableNoCatch Bool where
  propertyNoCatch = propertyNoCatch . liftBool

instance TestableNoCatch Result where
  propertyNoCatch = MkProperty . return . MkProp . return

instance TestableNoCatch Prop where
  propertyNoCatch p = MkProperty . return $ p

instance TestableNoCatch prop => TestableNoCatch (Gen prop) where
  propertyNoCatch mp = MkProperty $ do p <- mp; unProperty (againNoCatch $ propertyNoCatch p)

instance TestableNoCatch Property where
  propertyNoCatch p = p

againNoCatch :: Property -> Property
againNoCatch (MkProperty gen) = MkProperty $ do
  MkProp rose <- gen
  return . MkProp $ fmap (\res -> res{ abort = False }) rose
