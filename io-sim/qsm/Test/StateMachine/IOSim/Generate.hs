{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- |

module Test.StateMachine.IOSim.Generate
  ( forAllCommands
  , forAllParallelCommands
  , forAllNParallelCommands
  ) where

import Control.Monad.State
import Data.Bifunctor
import Data.List qualified as L
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Test.QuickCheck
import Test.StateMachine.IOSim.Types (StateMachine (..))
import Test.StateMachine.Logic
import Test.StateMachine.Types hiding (StateMachine (..))
import Test.StateMachine.Types.Rank2 qualified as Rank2
import Test.StateMachine.Utils
import Text.Show.Pretty hiding (reify)

forAllCommands :: Testable prop
               => (Show (cmd Symbolic), Show (resp Symbolic), Show (model Symbolic))
               => (Rank2.Traversable cmd, Rank2.Foldable resp)
               => StateMachine model cmd sut resp
               -> Maybe Int -- ^ Minimum number of commands.
               -> (Commands cmd resp -> prop)     -- ^ Predicate.
               -> Property
forAllCommands sm mminSize =
  forAllShrinkShow (generateCommands sm mminSize) (shrinkCommands sm) ppShow

deadlockError :: (Show (model Symbolic), Show (cmd Symbolic), Show (resp Symbolic))
              => model Symbolic -> [Command cmd resp] -> String -> b
deadlockError model generated counterexamples = error $ concat
  [ "\n"
  , "A deadlock occured while generating commands.\n"
  , "No pre-condition holds in the following model:\n\n"
  , "    " ++ ppShow model
  , "\n\nThe following commands have been generated so far:\n\n"
  , "    " ++ ppShow generated
  , "\n\n"
  , "Example commands generated whose pre-condition doesn't hold:\n\n"
  , "    " ++ counterexamples
  , "\n"
  ]

generateCommands :: (Rank2.Foldable resp, Show (model Symbolic))
                 => (Show (cmd Symbolic), Show (resp Symbolic))
                 => StateMachine model cmd sut resp
                 -> Maybe Int -- ^ Minimum number of commands.
                 -> Gen (Commands cmd resp)
generateCommands sm@StateMachine {..} mminSize =
  evalStateT (generateCommandsState sm newCounter mminSize) initModel

generateCommandsState :: forall model cmd sut resp. Rank2.Foldable resp
                      => (Show (model Symbolic), Show (cmd Symbolic), Show (resp Symbolic))
                      => StateMachine model cmd sut resp
                      -> Counter
                      -> Maybe Int -- ^ Minimum number of commands.
                      -> StateT (model Symbolic) Gen (Commands cmd resp)
generateCommandsState StateMachine {..} counter0 mminSize = do
  let minSize = fromMaybe 0 mminSize
  size0 <- lift (sized (\k -> choose (minSize, k + minSize)))
  go size0 counter0 []
  where
    go :: Int -> Counter -> [Command cmd resp]
       -> StateT (model Symbolic) Gen (Commands cmd resp)
    go 0    _       cmds = return (Commands (reverse cmds))
    go size counter cmds = do
      model <- get
      case generator model of
        Nothing  -> return (Commands (reverse cmds))
        Just gen -> do
          enext <- lift $ gen `suchThatEither` (boolean . precondition model)
          case enext of
            Left  ces  -> deadlockError model (reverse cmds) (ppShow ces)
            Right next -> do
              let (resp, counter') = runGenSym (mock model next) counter
              put (transition model next resp)
              go (size - 1) counter' (Command next resp (getUsedVars resp) : cmds)

getUsedVars :: Rank2.Foldable f => f Symbolic -> [Var]
getUsedVars = Rank2.foldMap (\(Symbolic v) -> [v])

-- | Shrink commands in a pre-condition and scope respecting way.
shrinkCommands ::  forall model cmd sut resp. (Rank2.Traversable cmd, Rank2.Foldable resp)
               => StateMachine model cmd sut resp -> Commands cmd resp
               -> [Commands cmd resp]
shrinkCommands sm@StateMachine {..} =
    concatMap go . shrinkListS' . unCommands
  where
    go :: Shrunk [Command cmd resp] -> [Commands cmd resp]
    go (Shrunk shrunk cmds) = map snd $
        shrinkAndValidate sm
                          (if shrunk then DontShrink else MustShrink)
                          (initValidateEnv initModel)
                          (Commands cmds)

-- | Environment required during 'shrinkAndValidate'
data ValidateEnv model = ValidateEnv {
      -- | The model we're starting validation from
      veModel   :: model Symbolic

      -- | Reference renumbering
      --
      -- When a command
      --
      -- > Command .. [Var i, ..]
      --
      -- is changed during validation to
      --
      -- > Command .. [Var j, ..]
      --
      -- then any subsequent uses of @Var i@ should be replaced by @Var j@. This
      -- is recorded in 'veScope'. When we /remove/ the first command
      -- altogether (during shrinking), then @Var i@ won't appear in the
      -- 'veScope' and shrank candidates that contain commands referring to @Var
      -- i@ should be considered as invalid.
    , veScope   :: Map Var Var

      -- | Counter (for generating new references)
    , veCounter :: Counter
    }

initValidateEnv :: model Symbolic -> ValidateEnv model
initValidateEnv initModel = ValidateEnv {
      veModel   = initModel
    , veScope   = M.empty
    , veCounter = newCounter
    }

data ShouldShrink = MustShrink | DontShrink

-- | Validate list of commands, optionally shrinking one of the commands
--
-- The input to this function is a list of commands ('Commands'), for example
--
-- > [A, B, C, D, E, F, G, H]
--
-- The /result/ is a /list/ of 'Commands', i.e. a list of lists. The
-- outermost list is used for all the shrinking possibilities. For example,
-- let's assume we haven't shrunk something yet, and therefore need to shrink
-- one of the commands. Let's further assume that only commands B and E can be
-- shrunk, to B1, B2 and E1, E2, E3 respectively. Then the result will look
-- something like
--
-- > [    -- outermost list recording all the shrink possibilities
-- >     [A', B1', C', D', E' , F', G', H']   -- B shrunk to B1
-- >   , [A', B2', C', D', E' , F', G', H']   -- B shrunk to B2
-- >   , [A', B' , C', D', E1', F', G', H']   -- E shrunk to E1
-- >   , [A', B' , C', D', E2', F', G', H']   -- E shrunk to E2
-- >   , [A', B' , C', D', E3', F', G', H']   -- E shrunk to E3
-- > ]
--
-- where one of the commands has been shrunk and all commands have been
-- validated and renumbered (references updated). So, in this example, the
-- result will contain at most 5 lists; it may contain fewer, since some of
-- these lists may not be valid.
--
-- If we _did_ already shrink something, then no commands will be shrunk, and
-- the resulting list will either be empty (if the list of commands was invalid)
-- or contain a /single/ element with the validated and renumbered commands.
shrinkAndValidate :: forall model cmd sut resp. (Rank2.Traversable cmd, Rank2.Foldable resp)
                  => StateMachine model cmd sut resp
                  -> ShouldShrink
                  -> ValidateEnv model
                  -> Commands cmd resp
                  -> [(ValidateEnv model, Commands cmd resp)]
shrinkAndValidate StateMachine {..} =
    \env shouldShrink cmds -> map (second Commands) $ go env shouldShrink (unCommands cmds)
  where
    go :: ShouldShrink -> ValidateEnv model -> [Command cmd resp] -> [(ValidateEnv model, [Command cmd resp])]
    go MustShrink   _   [] = []          -- we failed to shrink anything
    go DontShrink   env [] = [(env, [])] -- successful termination
    go shouldShrink (ValidateEnv model scope counter) (Command cmd' _resp vars' : cmds) =
      case Rank2.traverse (remapVars scope) cmd' of
        Just remapped ->
          -- shrink at most one command
          let candidates :: [(ShouldShrink, cmd Symbolic)]
              candidates =
                case shouldShrink of
                  DontShrink -> [(DontShrink, remapped)]
                  MustShrink -> map (DontShrink,) (shrinker model remapped)
                             ++ [(MustShrink, remapped)]
          in flip concatMap candidates $ \(shouldShrink', cmd) ->
               if boolean (precondition model cmd)
                 then let (resp, counter') = runGenSym (mock model cmd) counter
                          vars = getUsedVars resp
                          env' = ValidateEnv {
                                     veModel   = transition model cmd resp
                                   , veScope   = M.fromList (zip vars' vars) `M.union` scope
                                   , veCounter = counter'
                                   }
                      in map (second (Command cmd resp vars:)) $ go shouldShrink' env' cmds
                 else []
        Nothing ->
          []

    remapVars :: Map Var Var -> Symbolic a -> Maybe (Symbolic a)
    remapVars scope (Symbolic v) = Symbolic <$> M.lookup v scope


------------------------------------------------------------------------

forAllParallelCommands :: Testable prop
                       => (Show (cmd Symbolic), Show (resp Symbolic), Show (model Symbolic))
                       => (Rank2.Traversable cmd, Rank2.Foldable resp)
                       => StateMachine model cmd sut resp
                       -> Maybe Int
                       -> (ParallelCommands cmd resp -> prop)     -- ^ Predicate.
                       -> Property
forAllParallelCommands sm mminSize prop =
  forAllShrinkShow (generateParallelCommands sm mminSize) (shrinkParallelCommands sm) ppShow prop

forAllNParallelCommands :: Testable prop
                        => (Show (cmd Symbolic), Show (resp Symbolic), Show (model Symbolic))
                        => (Rank2.Traversable cmd, Rank2.Foldable resp)
                        => StateMachine model cmd sut resp
                        -> Int                                      -- ^ Number of threads
                        -> (NParallelCommands cmd resp -> prop)     -- ^ Predicate.
                        -> Property
forAllNParallelCommands sm np prop =
  forAllShrinkShow (generateNParallelCommands sm np) (shrinkNParallelCommands sm) ppShow prop

-- | Generate parallel commands.
--
-- Parallel commands are generated as follows. We begin by generating
-- sequential commands and then splitting this list in two at some index. The
-- first half will be used as the prefix.
--
-- The second half will be used to build suffixes. For example, starting from
-- the following sequential commands:
--
-- > [A, B, C, D, E, F, G, H, I]
--
-- We split it in two, giving us the prefix and the rest:
--
-- > prefix: [A, B]
-- > rest:   [C, D, E, F, G, H, I]
--
-- We advance the model with the prefix.
--
-- __Make a suffix__: we take commands from @rest@ as long as these are
-- parallel safe (see 'parallelSafe'). This means that the pre-conditions
-- (using the \'advanced\' model) of each of those commands will hold no
-- matter in which order they are executed.
--
-- Say this is true for @[C, D, E]@, but not anymore for @F@, maybe because
-- @F@ depends on one of @[C, D, E]@. Then we divide this \'chunk\' in two by
-- splitting it in the middle, obtaining @[C]@ and @[D, E]@. These two halves
-- of the chunk (stored as a 'Pair') will later be executed in parallel.
-- Together they form one suffix.
--
-- Then the model is advanced using the whole chunk @[C, D, E]@. Think of it
-- as a barrier after executing the two halves of the chunk in parallel. Then
-- this process of building a chunk/suffix repeats itself, starting from
-- __Make a suffix__ using the \'advanced\' model.
--
-- In the end we might end up with something like this:
--
-- >         ┌─ [C] ──┐  ┌ [F, G] ┐
-- > [A, B] ─┤        ├──┤        │
-- >         └ [D, E] ┘  └ [H, I] ┘
--
generateParallelCommands :: forall model cmd sut resp. Rank2.Foldable resp
                         => Show (model Symbolic)
                         => (Show (cmd Symbolic), Show (resp Symbolic))
                         => StateMachine model cmd sut resp
                         -> Maybe Int
                         -> Gen (ParallelCommands cmd resp)
generateParallelCommands sm@StateMachine { initModel } mminSize  = do
  Commands cmds      <- generateCommands sm mminSize
  prefixLength       <- sized (\k -> choose (0, k `div` 3))
  let (prefix, rest) =  bimap Commands Commands (splitAt prefixLength cmds)
  return (ParallelCommands prefix
            (makeSuffixes (advanceModel sm initModel prefix) rest))
  where
    makeSuffixes :: model Symbolic -> Commands cmd resp -> [Pair (Commands cmd resp)]
    makeSuffixes model0 = go model0 [] . unCommands
      where
        go _     acc []   = reverse acc
        go model acc cmds = go (advanceModel sm model (Commands safe))
                               (Pair (Commands safe1) (Commands safe2) : acc)
                               rest
          where
            (safe, rest)   = spanSafe sm model [] cmds
            (safe1, safe2) = splitAt (length safe `div` 2) safe

-- Split the list of commands in two such that the first half is a
-- list of commands for which the preconditions of all commands hold
-- for permutation of the list, i.e. it is parallel safe. The other
-- half is the remainder of the input list.
spanSafe :: Rank2.Foldable resp
         => StateMachine model cmd sut resp
         -> model Symbolic -> [Command cmd resp] -> [Command cmd resp]
         -> ([Command cmd resp], [Command cmd resp])
spanSafe _ _     safe []           = (reverse safe, [])
spanSafe sm model safe (cmd : cmds)
    | length safe <= 5
  , parallelSafe sm model (Commands (cmd : safe))
  = spanSafe sm model (cmd : safe) cmds
  | otherwise
  = (reverse safe, cmd : cmds)

-- Generate Parallel commands. The length of each suffix, indicates how many thread can
-- concurrently execute the commands safely.
generateNParallelCommands :: forall model cmd sut resp. Rank2.Foldable resp
                          => Show (model Symbolic)
                          => (Show (cmd Symbolic), Show (resp Symbolic))
                          => StateMachine model cmd sut resp
                          -> Int
                          -> Gen (NParallelCommands cmd resp)
generateNParallelCommands sm@StateMachine { initModel } np =
  if np <= 0 then error "number of threads must be positive" else do
  Commands cmds      <- generateCommands sm Nothing
  prefixLength       <- sized (\k -> choose (0, k `div` 3))
  let (prefix, rest) =  bimap Commands Commands (splitAt prefixLength cmds)
  return (ParallelCommands prefix
            (makeSuffixes (advanceModel sm initModel prefix) rest))
  where
    makeSuffixes :: model Symbolic -> Commands cmd resp -> [[Commands cmd resp]]
    makeSuffixes model0 = go model0 [] . unCommands
      where
        go :: model Symbolic
           -> [[Commands cmd resp]]
           -> [Command cmd resp]
           -> [[Commands cmd resp]]
        go _     acc []   = reverse acc
        go model acc cmds = go (advanceModel sm model (Commands safe))
                               (safes : acc)
                               rest
          where
            (safe, rest)   = spanSafe sm model [] cmds
            safes = Commands <$> chunksOf np (length safe `div` np) safe

        -- Split the list in n sublists, whose concat is the initial list.
        -- We try to keep the length of each sublist len.
        --
        -- It is important that we miss no elements here or else executeCommands may fail, because
        -- of missing references. It is also important that the final list has the correct length
        -- n, or else there will be different number of threads than the user specified.
        chunksOf :: Int -> Int -> [a] -> [[a]]
        chunksOf 1 _ xs = [xs]
        chunksOf n len xs = as : chunksOf (n-1) len bs
            where (as, bs) = splitAt len xs


-- | A list of commands is parallel safe if the pre-conditions for all commands
--   hold in all permutations of the list.
parallelSafe :: Rank2.Foldable resp
             => StateMachine model cmd sut resp -> model Symbolic
             -> Commands cmd resp -> Bool
parallelSafe StateMachine { precondition, transition, mock } model0
  = all (preconditionsHold model0)
  . L.permutations
  . unCommands
  where
    preconditionsHold _     []                             = True
    preconditionsHold model (Command cmd resp vars : cmds) =
        boolean (precondition model cmd) &&
          preconditionsHold (transition model cmd resp) cmds &&
          -- This makes sure that in all permutations the length of variables created is the same.
          -- By doing so, we try to avoid MockSemanticsMismatch errors.
          -- More https://github.com/advancedtelematic/quickcheck-state-machine/pull/348
          length vars == length (getUsedVars $ fst $ runGenSym (mock model cmd) newCounter)

-- | Apply the transition of some commands to a model.
advanceModel :: StateMachine model cmd sut resp
             -> model Symbolic      -- ^ The model.
             -> Commands cmd resp   -- ^ The commands.
             -> model Symbolic
advanceModel StateMachine { transition } model0 =
  go model0 . unCommands
  where
    go model []                              = model
    go model (Command cmd resp _vars : cmds) =
        go (transition model cmd resp) cmds

------------------------------------------------------------------------

-- | Shrink a parallel program in a pre-condition and scope respecting
--   way.
shrinkParallelCommands
  :: forall cmd model sut resp. Rank2.Traversable cmd
  => Rank2.Foldable resp
  => StateMachine model cmd sut resp
  -> (ParallelCommands cmd resp -> [ParallelCommands cmd resp])
shrinkParallelCommands sm (ParallelCommands prefix suffixes)
  = concatMap go
      [ Shrunk s (ParallelCommands prefix' (map toPair suffixes'))
      | Shrunk s (prefix', suffixes') <- shrinkPairS shrinkCommands' shrinkSuffixes
                                                     (prefix, map fromPair suffixes)
      ]
      ++
      shrinkMoveSuffixToPrefix
  where
    go :: Shrunk (ParallelCommands cmd resp) -> [ParallelCommands cmd resp]
    go (Shrunk shrunk cmds) =
        shrinkAndValidateParallel sm
                                  (if shrunk then DontShrink else MustShrink)
                                  cmds

    shrinkSuffixes :: [(Commands cmd resp, Commands cmd resp)]
                   -> [Shrunk [(Commands cmd resp, Commands cmd resp)]]
    shrinkSuffixes = shrinkListS (shrinkPairS' shrinkCommands')

    -- Moving a command from a suffix to the prefix preserves validity
    shrinkMoveSuffixToPrefix :: [ParallelCommands cmd resp]
    shrinkMoveSuffixToPrefix = case suffixes of
      []                   -> []
      (suffix : suffixes') ->
        [ ParallelCommands (prefix <> Commands [prefix'])
                           (fmap Commands (toPair suffix') : suffixes')
        | (prefix', suffix') <- pickOneReturnRest2 (unCommands (proj1 suffix),
                                                    unCommands (proj2 suffix))
        ]

-- | Shrink a parallel program in a pre-condition and scope respecting
--   way.
shrinkNParallelCommands
  :: forall cmd model sut resp. Rank2.Traversable cmd
  => Rank2.Foldable resp
  => StateMachine model cmd sut resp
  -> (NParallelCommands cmd resp -> [NParallelCommands cmd resp])
shrinkNParallelCommands sm (ParallelCommands prefix suffixes)
  = concatMap go
      [ Shrunk s (ParallelCommands prefix' suffixes')
      | Shrunk s (prefix', suffixes') <- shrinkPairS shrinkCommands' shrinkSuffixes
                                                     (prefix, suffixes)
      ]
      ++
      shrinkMoveSuffixToPrefix
  where
    go :: Shrunk (NParallelCommands cmd resp) -> [NParallelCommands cmd resp]
    go (Shrunk shrunk cmds) =
      shrinkAndValidateNParallel sm
                                       (if shrunk then DontShrink else MustShrink)
                                       cmds

    shrinkSuffixes :: [[Commands cmd resp]]
                   -> [Shrunk [[Commands cmd resp]]]
    shrinkSuffixes = shrinkListS (shrinkListS'' shrinkCommands')

    -- Moving a command from a suffix to the prefix preserves validity
    shrinkMoveSuffixToPrefix :: [NParallelCommands cmd resp]
    shrinkMoveSuffixToPrefix = case suffixes of
      []                   -> []
      (suffix : suffixes') ->
        [ ParallelCommands (prefix <> Commands [prefix'])
                           (fmap Commands suffix' : suffixes')
        | (prefix', suffix') <- pickOneReturnRestL (unCommands <$> suffix)
        ]

-- | Shrinks Commands in a way that it has strictly less number of commands.
shrinkCommands' :: Commands cmd resp -> [Shrunk (Commands cmd resp)]
shrinkCommands' = map (fmap Commands) . shrinkListS' . unCommands

shrinkAndValidateParallel :: forall model cmd sut resp. (Rank2.Traversable cmd, Rank2.Foldable resp)
                          => StateMachine model cmd sut resp
                          -> ShouldShrink
                          -> ParallelCommands cmd resp
                          -> [ParallelCommands cmd resp]
shrinkAndValidateParallel sm@StateMachine { initModel } = \shouldShrink (ParallelCommands prefix suffixes) ->
    let env = initValidateEnv initModel
        curryGo shouldShrink' (env', prefix') = go prefix' env' shouldShrink' suffixes in
    case shouldShrink of
      DontShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate sm DontShrink env prefix)
      MustShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate sm MustShrink env prefix)
                 ++ concatMap (curryGo MustShrink) (shrinkAndValidate sm DontShrink env prefix)
  where
    go :: Commands cmd resp          -- validated prefix
       -> ValidateEnv model          -- environment after the prefix
       -> ShouldShrink               -- should we /still/ shrink something?
       -> [Pair (Commands cmd resp)] -- suffixes to validate
       -> [ParallelCommands cmd resp]
    go prefix' = go' []
      where
        go' :: [Pair (Commands cmd resp)] -- accumulated validated suffixes (in reverse order)
            -> ValidateEnv model          -- environment after the validated suffixes
            -> ShouldShrink               -- should we /still/ shrink something?
            -> [Pair (Commands cmd resp)] -- suffixes to validate
            -> [ParallelCommands cmd resp]
        go' _   _   MustShrink [] = [] -- Failed to shrink something
        go' acc _   DontShrink [] = [ParallelCommands prefix' (reverse acc)]
        go' acc env shouldShrink (Pair l r : suffixes) = do
            ((shrinkL, shrinkR), shrinkRest) <- shrinkOpts
            (envL, l') <- shrinkAndValidate sm shrinkL  env                         l
            (envR, r') <- shrinkAndValidate sm shrinkR (env `withCounterFrom` envL) r
            go' (Pair l' r' : acc) (combineEnv sm envL envR r') shrinkRest suffixes
          where

            shrinkOpts :: [((ShouldShrink, ShouldShrink), ShouldShrink)]
            shrinkOpts =
                case shouldShrink of
                  DontShrink -> [ ((DontShrink, DontShrink), DontShrink) ]
                  MustShrink -> [ ((MustShrink, DontShrink), DontShrink)
                                , ((DontShrink, MustShrink), DontShrink)
                                , ((DontShrink, DontShrink), MustShrink) ]

combineEnv :: StateMachine model cmd sut resp
           -> ValidateEnv model
           -> ValidateEnv model
           -> Commands cmd resp
           -> ValidateEnv model
combineEnv sm envL envR cmds = ValidateEnv {
      veModel   = advanceModel sm (veModel envL) cmds
    , veScope   = M.union (veScope envL) (veScope envR)
    , veCounter = veCounter envR
    }

withCounterFrom :: ValidateEnv model -> ValidateEnv model -> ValidateEnv model
withCounterFrom e e' = e { veCounter = veCounter e' }

shrinkAndValidateNParallel :: forall model cmd sut resp. (Rank2.Traversable cmd, Rank2.Foldable resp)
                           => StateMachine model cmd sut resp
                           -> ShouldShrink
                           -> NParallelCommands cmd resp
                           -> [NParallelCommands cmd resp]
shrinkAndValidateNParallel sm = \shouldShrink  (ParallelCommands prefix suffixes) ->
    let env = initValidateEnv $ initModel sm
        curryGo shouldShrink' (env', prefix') = go prefix' env' shouldShrink' suffixes in
    case shouldShrink of
      DontShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate sm DontShrink env prefix)
      MustShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate sm MustShrink env prefix)
                 ++ concatMap (curryGo MustShrink) (shrinkAndValidate sm DontShrink env prefix)
  where

    go :: Commands cmd resp         -- validated prefix
       -> ValidateEnv model         -- environment after the prefix
       -> ShouldShrink              -- should we /still/ shrink something?
       -> [[Commands cmd resp]]     -- suffixes to validate
       -> [NParallelCommands cmd resp]
    go prefix' = go' []
      where
        go' :: [[Commands cmd resp]] -- accumulated validated suffixes (in reverse order)
            -> ValidateEnv model     -- environment after the validated suffixes
            -> ShouldShrink          -- should we /still/ shrink something?
            -> [[Commands cmd resp]] -- suffixes to validate
            -> [NParallelCommands cmd resp]
        go' _   _   MustShrink [] = [] -- Failed to shrink something
        go' acc _   DontShrink [] = [ParallelCommands prefix' (reverse acc)]
        go' acc env shouldShrink (suffix : suffixes) = do
            (suffixWithShrinks, shrinkRest) <- shrinkOpts suffix
            (envFinal, suffix') <- snd $ foldl f (True, [(env,[])]) suffixWithShrinks
            go' (reverse suffix' : acc) envFinal shrinkRest suffixes
          where

            f :: (Bool, [(ValidateEnv model, [Commands cmd resp])])
              -> (ShouldShrink, Commands cmd resp)
              -> (Bool, [(ValidateEnv model, [Commands cmd resp])])
            f (firstCall, acc') (toShrink, cmds) = (False, acc'')
              where
                    acc'' = do
                      (envPrev, cmdsPrev) <- acc'
                      let envUsed = if firstCall then env else env `withCounterFrom` envPrev
                      (env', cmd') <- shrinkAndValidate sm toShrink envUsed cmds
                      let env'' = if firstCall then env' else
                            combineEnv sm envPrev env' cmd'
                      return (env'', cmd' : cmdsPrev)

            shrinkOpts :: [a] -> [([(ShouldShrink, a)], ShouldShrink)]
            shrinkOpts ls =
              let len = length ls
                  dontShrink = replicate len DontShrink
                  shrinks = if len == 0
                    then error "Invariant violation! A suffix should never be an empty list"
                    else flip map [1..len] $ \n ->
                        replicate (n - 1) DontShrink ++ [MustShrink] ++ replicate (len - n) DontShrink
              in case shouldShrink of
                  DontShrink -> [(zip dontShrink ls, DontShrink)]
                  MustShrink -> fmap (\shrinkLs -> (zip shrinkLs ls, DontShrink)) shrinks
                             ++ [(zip dontShrink ls, MustShrink)]
