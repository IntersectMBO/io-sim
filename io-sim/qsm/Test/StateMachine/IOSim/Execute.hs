{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.StateMachine.IOSim.Execute
  ( -- * Executing
    runSequential
  , runParallel
  , runParallelException
  , runSequentialPOR
  , runSequentialPORException
  ) where

import Control.Concurrent.Class.MonadSTM hiding (check)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim
import Data.Bifunctor
import Data.Dynamic (Dynamic, toDyn)
import Data.Either
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Maybe
import Test.QuickCheck
import Test.StateMachine.IOSim.Types
import Test.StateMachine.Logic
import Test.StateMachine.Types hiding (StateMachine (..))
import Test.StateMachine.Types.Rank2 qualified as Rank2
import Text.Show.Pretty hiding (reify)

{-------------------------------------------------------------------------------
  Runner helpers
-------------------------------------------------------------------------------}

-- | Run an IOSim action
runIOSim ::
    (Show (resp Concrete)) =>
    (forall s. IOSim s (resp Concrete)) ->
    ( SimTrace (resp Concrete)
    , -- \^ Trace for printing
      resp Concrete
    , -- \^ Response
      Property
      -- \^ False if there was an exception thrown
    )
runIOSim act =
    let trace = runSimTrace act
        e msg err =
            ( trace
            , undefined
            , counterexample (ppTrace trace) $
                counterexample (msg <> show err) $
                    property False
            )
     in case traceResult False trace of
            Left (FailureException err) -> e "Exception thrown: " err
            Left err                    -> e "Internal IOSim error: " err
            Right v                     -> (trace, v, property True)

-- | Run an IOSimPOR action and check the property for each possible interleaving
runIOSimPOR ::
    (Show a) =>
    -- | Test to run
    (forall s. IOSim s a) ->
    ( SimTrace a ->
      -- \^ Trace
      Bool ->
      -- \^ Was there a previous successful trace?
      a ->
      -- \^ traceResult
      Property
    ) ->
    Property
runIOSimPOR act prop =
    exploreSimTrace id (exploreRaces >> act) $ \(isJust -> pt) trace ->
        let e msg err =
                counterexample (ppTrace trace) $
                    counterexample (msg <> show err) $
                        property False
         in case traceResult False trace of
                Left (FailureException err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Exception thrown: "
                        )
                        err
                Left (FailureDeadlock err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Deadlock found: "
                        )
                        err
                Left err ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Internal IOSim error: "
                        )
                        err
                Right v -> prop trace pt v

-- | Run an IOSimPOR action and check the property for each possible interleaving
runIOSimPORAcceptExceptions ::
    (Show a) =>
    -- | Test to run
    (forall s. IOSim s a) ->
    ( SimTrace a ->
      -- \^ Trace
      Bool ->
      -- \^ Was there a previous successful trace?
      Either SomeException a ->
      -- \^ traceResult
      Property
    ) ->
    Property
runIOSimPORAcceptExceptions act prop =
    exploreSimTrace id (exploreRaces >> act) $ \(isJust -> pt) trace ->
        let e msg err =
                counterexample (ppTrace trace) $
                    counterexample (msg <> show err) $
                        property False
         in case traceResult False trace of
                Left (FailureDeadlock err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Deadlock found: "
                        )
                        err
                Right v -> prop trace pt (Right v)
                Left (FailureException err) -> prop trace pt (Left err)
                Left err ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Internal IOSim error: "
                        )
                        err

{-------------------------------------------------------------------------------
  Running functions
-------------------------------------------------------------------------------}

{- | Run a sequence of commands sequentially

Checks for:

 * precondition

 * exceptions

 * postcondition

 * invariant

 * mock semantics match
-}
runSequential ::
    forall cmd resp sut model.
    (Show (cmd Concrete), Show (resp Concrete)) =>
    (Rank2.Traversable cmd, Rank2.Foldable resp) =>
    StateMachine model cmd sut resp ->
    Commands cmd resp ->
    Property
runSequential sm@StateMachine{..} (Commands scmds) =
    foldl' act (property True, [], emptyEnvironment, initModel, newCounter, initModel) scmds
        & (\(p, _, _, _, _, _) -> p)
  where
    act (prevProp, prevCmds, env, smodel, counter, cmodel) (Command scmd _ vars) =
        let
            -- Precondition
            pPre = prec sm smodel scmd

            -- Simulate
            ccmd = fromRight (error "runSequential: impossible") (reify env scmd)
            prevCmds' = prevCmds ++ [ccmd]
            (trace, cresp, prop) = runIOSim $ do
                sut <- initSut
                sequence_ $ map (\c -> semantics sut c) prevCmds
                semantics sut ccmd

            -- Postcondition and invariant
            pPost = post sm cmodel ccmd cresp False trace
            pInv = inv sm cmodel False trace

            -- Mock
            (pMock, cvars) = mockCheck ccmd cresp vars
            (sresp, counter') = runGenSym (mock smodel scmd) counter
         in
            ( prevProp .&&. pPre .&&. prop .&&. pMock .&&. pPost .&&. pInv
            , prevCmds'
            , insertConcretes vars cvars env
            , transition smodel scmd sresp
            , counter'
            , transition cmodel ccmd cresp
            )

{- | Run a sequence of commands sequentially, additionally checking for races.

Checks for:

 * precondition

 * exceptions

 * postcondition

 * invariant

 * mock semantics match

For all possible races found, the postcondition and invariant must hold.
-}
runSequentialPOR ::
    forall cmd resp sut model.
    (Show (cmd Concrete), Show (resp Concrete)) =>
    (Rank2.Traversable cmd, Rank2.Foldable resp) =>
    StateMachine model cmd sut resp ->
    Commands cmd resp ->
    Property
runSequentialPOR sm@StateMachine{..} (Commands scmds) =
    foldl' act (property True, [], emptyEnvironment, initModel, newCounter, initModel) scmds
        & (\(p, _, _, _, _, _) -> p)
  where
    act (prevProp, prevCmds, env, smodel, counter, cmodel) (Command scmd _ vars) =
        let
            -- Precondition
            pPre = prec sm smodel scmd

            -- Simulation
            ccmd = fromRight (error "runSequential: impossible") (reify env scmd)
            prevCmds' = prevCmds ++ [ccmd]
            simAction = do
                sut <- initSut
                sequence_ $ map (\c -> semantics sut c) prevCmds
                semantics sut ccmd
            -- Parallel property (for all interleavings)
            porProp = runIOSimPOR simAction $ \trace pt cresp' ->
                let
                    (pMock, _) = mockCheck ccmd cresp' vars
                    pPost = post sm cmodel ccmd cresp' pt trace
                    pInv = inv sm cmodel pt trace
                 in
                    prevProp .&&. pPre .&&. pMock .&&. pPost .&&. pInv

            -- Advance model
            (_, cresp, _) = runIOSim simAction
            (_, cvars) = mockCheck ccmd cresp vars
            (sresp, counter') = runGenSym (mock smodel scmd) counter
         in
            ( prevProp .&&. porProp
            , prevCmds'
            , insertConcretes vars cvars env
            , transition smodel scmd sresp
            , counter'
            , transition cmodel ccmd cresp
            )

runSequentialPORException ::
    forall cmd resp sut model.
    (Show (cmd Concrete), Show (resp Concrete)) =>
    (Rank2.Traversable cmd, Rank2.Foldable resp) =>
    StateMachine model cmd sut resp ->
    Commands cmd resp ->
    Property
runSequentialPORException sm@StateMachine{..} (Commands scmds) =
    foldl' act (property True, [], emptyEnvironment, initModel, newCounter, initModel) scmds
        & (\(p, _, _, _, _, _) -> p)
  where
    act (prevProp, prevCmds, env, smodel, counter, cmodel) (Command scmd _ vars) =
        let
            -- Precondition
            pPre = prec sm smodel scmd

            -- Simulation
            ccmd = fromRight (error "runSequential: impossible") (reify env scmd)
            prevCmds' = prevCmds ++ [ccmd]
            simAction = do
                sut <- initSut
                sequence_ $ map (\c -> semantics sut c) prevCmds
                t1 <- async $ semantics sut ccmd
                canceller <- async (cancel t1)
                wait canceller
                wait t1
            -- Parallel property (for all interleavings)
            porProp = runIOSimPORAcceptExceptions simAction $ \trace pt -> \case
              Right cresp' ->
                let
                    (pMock, _) = mockCheck ccmd cresp' vars
                    pPost = post sm cmodel ccmd cresp' pt trace
                    pInv = inv sm cmodel pt trace
                 in
                    prevProp .&&. pPre .&&. pMock .&&. pPost .&&. pInv
              Left _ -> property True

            -- Advance model
            (_, cresp, _) = runIOSim simAction
            (_, cvars) = mockCheck ccmd cresp vars
            (sresp, counter') = runGenSym (mock smodel scmd) counter
         in
            ( prevProp .&&. porProp
            , prevCmds'
            , insertConcretes vars cvars env
            , transition smodel scmd sresp
            , counter'
            , transition cmodel ccmd cresp
            )

{-------------------------------------------------------------------------------
  Parallel
-------------------------------------------------------------------------------}

data OnePairF f a = One a | PP (f a)

-- | Run a ParallelCommands
runParallel ::
    forall cmd resp sut model.
    (Show (cmd Concrete), Show (resp Concrete)) =>
    (Rank2.Traversable cmd, Rank2.Foldable resp) =>
    StateMachine model cmd sut resp ->
    ParallelCommands cmd resp ->
    Property
runParallel sm@StateMachine{..} (ParallelCommands pref suff) =
    foldl' act (property True, [], emptyEnvironment, initModel, newCounter, initModel) (One pref : map PP suff)
        & (\(p, _, _, _, _, _) -> p)
  where
    act st (One (Commands scmds)) = foldl' runSequential' st scmds
    act (prevProp, prevCmds, env, _smodel, _counter, cmodel) (PP (Pair (Commands p1) (Commands p2))) =
        let
            -- Reify commands and create environments
            --
            -- The only way to do this is to run the actions in two separate IOSim
            -- simulations, accumulating the environment and commands.
            --
            -- Due to the iterative nature of this test, we can be confident this will
            -- not fail if previous steps did not fail.
            reifyCmd (acc, env') (Command scmd _ vars) =
                let ccmd = fromRight (error "runSequential: impossible") (reify env' scmd)
                    simAction' = do
                        sut <- initSut
                        sequence_ $ map (semantics sut) (prevCmds ++ map fst acc)
                        semantics sut ccmd
                    (_, cresp, _) = runIOSim simAction'
                    (_, cvars) = mockCheck ccmd cresp vars
                 in (acc ++ [(ccmd, cresp)], insertConcretes vars cvars env')

            (p1', env1) = foldl' reifyCmd ([], env) p1
            (p2', env2) = foldl' reifyCmd ([], env) p2

            -- The IOSimPOR parallel action
            simAction = do
                -- We create a TQueue to which we will send command + response pairs
                tq <- atomically $ do
                    tq <- newTQueue
                    labelTQueue tq "CONTROL"
                    pure tq
                sut <- initSut
                -- Run the sequential prefix
                sequence_ $ map (\c -> semantics sut c) prevCmds

                let racingAction =
                        async
                            . sequence_
                            . map
                                ( \ccmd -> do
                                    cresp <- semantics sut ccmd
                                    atomically $ writeTQueue tq (ccmd, cresp)
                                )
                            . map fst
                -- Run both actions in a race, and wait for both
                t1 <- racingAction p1'
                t2 <- racingAction p2'

                wait t1
                wait t2

                -- Get all command + response pairs
                atomically $ drainTQueue tq

            -- The simulation
            porProp = runIOSimPOR simAction $ \trace _ cmds ->
                let events = traceEvents trace
                    events' = [first reverse $ splitAt n events | n <- [0 .. length events]]
                    -- A real interleaving is one in which the thread that sent
                    -- the message to the TQueue has done some work since it was
                    -- scheduled, i.e. the TQueue writing action was not
                    -- artificially delayed
                    fakeInterleaving =
                        [ not $ hasDoneWork id2 before
                        | (before, (_, id2, _, EventTxCommitted w _ _) : _) <- events'
                        , Just "CONTROL" `elem` map l_label w -- has written to the CONTROL TQueue
                        ]
                 in counterexample (ppTrace trace) $
                        -- If this was a fake interleaving just make it pass
                        property (or fakeInterleaving)
                            .||. ( fst $
                                    foldl'
                                        ( \(prop, model) (cmd, resp) ->
                                            let model' = transition model cmd resp
                                                prop' = case logic (postcondition model cmd resp) of
                                                    VFalse ce -> counterexample ("Poscondition impossible! " <> show ce) $ property False
                                                    _ -> property True
                                             in (prop .&&. prop', model')
                                        )
                                        (property True, cmodel)
                                        cmds
                                 )
         in
            ( prevProp .&&. porProp
            , prevCmds ++ map fst p1' ++ map fst p2'
            , env1 <> env2
            , undefined
            , undefined
            , foldl (\x (y, z) -> transition x y z) cmodel $ p1' ++ p2'
            )

    -- This is almost the same as @runSequential@ but it accumulates the concrete
    -- commands.
    runSequential' (prevProp, prevCmds, env, smodel, counter, cmodel) (Command scmd _ vars) =
        let
            -- Precondition
            pPre = prec sm smodel scmd

            -- Simulate
            ccmd = fromRight (error "runSequential: impossible") (reify env scmd)
            prevCmds' = prevCmds ++ [ccmd]
            simAction = do
                sut <- initSut
                sequence_ $ map (\c -> semantics sut c) prevCmds
                semantics sut ccmd
            (trace, cresp, prop) = runIOSim simAction

            -- Postcondition
            pPost = post sm cmodel ccmd cresp False trace
            pInv = inv sm cmodel False trace

            -- Mock
            (pMock, cvars) = mockCheck ccmd cresp vars
            (sresp, counter') = runGenSym (mock smodel scmd) counter
         in
            ( prevProp .&&. pPre .&&. prop .&&. pPost .&&. pInv .&&. pMock
            , prevCmds'
            , insertConcretes vars cvars env
            , transition smodel scmd sresp
            , counter'
            , transition cmodel ccmd cresp
            )

-- | Run a ParallelCommands
runParallelException ::
    forall cmd resp sut model.
    (Show (cmd Concrete), Show (resp Concrete)) =>
    (Rank2.Traversable cmd, Rank2.Foldable resp) =>
    StateMachine model cmd sut resp ->
    ParallelCommands cmd resp ->
    Property
runParallelException sm@StateMachine{..} (ParallelCommands pref suff) =
    foldl' act (property True, [], emptyEnvironment, initModel, newCounter, initModel) (One pref : map PP suff)
        & (\(p, _, _, _, _, _) -> p)
  where
    act st (One (Commands scmds)) = foldl' runSequential' st scmds
    act (prevProp, prevCmds, env, _smodel, _counter, cmodel) (PP (Pair (Commands p1) (Commands p2))) =
        let
            -- Reify commands and create environments
            --
            -- The only way to do this is to run the actions in two separate IOSim
            -- simulations, accumulating the environment and commands.
            --
            -- Due to the iterative nature of this test, we can be confident this will
            -- not fail if previous steps did not fail.
            reifyCmd (acc, env') (Command scmd _ vars) =
                let ccmd = fromRight (error "runSequential: impossible") (reify env' scmd)
                    simAction' = do
                        sut <- initSut
                        sequence_ $ map (semantics sut) (prevCmds ++ map fst acc)
                        semantics sut ccmd
                    (_, cresp, _) = runIOSim simAction'
                    (_, cvars) = mockCheck ccmd cresp vars
                 in (acc ++ [(ccmd, cresp)], insertConcretes vars cvars env')

            (p1', env1) = foldl' reifyCmd ([], env) p1
            (p2', env2) = foldl' reifyCmd ([], env) p2

            -- The IOSimPOR parallel action
            simAction = do
                -- We create a TQueue to which we will send command + response pairs
                tq <- atomically $ do
                    tq <- newTQueue
                    labelTQueue tq "CONTROL"
                    pure tq
                sut <- initSut
                -- Run the sequential prefix
                sequence_ $ map (\c -> semantics sut c) prevCmds

                let racingAction =
                        async
                            . sequence_
                            . map
                                ( \ccmd -> do
                                    cresp <- semantics sut ccmd
                                    atomically $ writeTQueue tq (ccmd, cresp)
                                )
                            . map fst
                -- Run both actions in a race, and wait for both
                t1 <- racingAction p1'
                t2 <- racingAction p2'
                t3 <- async (cancel t1)
                wait t3
                wait t2
                wait t1

                -- Get all command + response pairs
                atomically $ drainTQueue tq

            -- The simulation
            porProp = runIOSimPORAcceptExceptions simAction $ \trace _ -> \case
              Left {} -> property True
              Right cmds ->
                let events = traceEvents trace
                    events' = [first reverse $ splitAt n events | n <- [0 .. length events]]
                    -- A real interleaving is one in which the thread that sent
                    -- the message to the TQueue has done some work since it was
                    -- scheduled, i.e. the TQueue writing action was not
                    -- artificially delayed
                    fakeInterleaving =
                        [ not $ hasDoneWork id2 before
                        | (before, (_, id2, _, EventTxCommitted w _ _) : _) <- events'
                        , Just "CONTROL" `elem` map l_label w -- has written to the CONTROL TQueue
                        ]
                 in counterexample (ppTrace trace) $
                        -- If this was a fake interleaving just make it pass
                        property (or fakeInterleaving)
                            .||. ( fst $
                                    foldl'
                                        ( \(prop, model) (cmd, resp) ->
                                            let model' = transition model cmd resp
                                                prop' = case logic (postcondition model cmd resp) of
                                                    VFalse ce -> counterexample ("Poscondition impossible! " <> show ce) $ property False
                                                    _ -> property True
                                             in (prop .&&. prop', model')
                                        )
                                        (property True, cmodel)
                                        cmds
                                 )
         in
            ( prevProp .&&. porProp
            , prevCmds ++ map fst p1' ++ map fst p2'
            , env1 <> env2
            , undefined
            , undefined
            , foldl (\x (y, z) -> transition x y z) cmodel $ p1' ++ p2'
            )

    -- This is almost the same as @runSequential@ but it accumulates the concrete
    -- commands.
    runSequential' (prevProp, prevCmds, env, smodel, counter, cmodel) (Command scmd _ vars) =
        let
            -- Precondition
            pPre = prec sm smodel scmd

            -- Simulate
            ccmd = fromRight (error "runSequential: impossible") (reify env scmd)
            prevCmds' = prevCmds ++ [ccmd]
            simAction = do
                sut <- initSut
                sequence_ $ map (\c -> semantics sut c) prevCmds
                semantics sut ccmd
            (trace, cresp, prop) = runIOSim simAction

            -- Postcondition
            pPost = post sm cmodel ccmd cresp False trace
            pInv = inv sm cmodel False trace

            -- Mock
            (pMock, cvars) = mockCheck ccmd cresp vars
            (sresp, counter') = runGenSym (mock smodel scmd) counter
         in
            ( prevProp .&&. pPre .&&. prop .&&. pPost .&&. pInv .&&. pMock
            , prevCmds'
            , insertConcretes vars cvars env
            , transition smodel scmd sresp
            , counter'
            , transition cmodel ccmd cresp
            )

hasDoneWork :: (Eq p) => p -> [(a, p, c, SimEventType)] -> Bool
hasDoneWork idX evs =
    go False evs
  where
    go acc [] = acc
    go acc ((_, idY, _, ev) : next)
        | idX /= idY = acc
        | otherwise =
            go
                ( acc || case ev of
                    EventThrow{}        -> True
                    EventThrowTo{}      -> True
                    EventThreadForked{} -> True
                    EventTxCommitted{}  -> True
                    EventTxAborted{}    -> True
                    EventTxBlocked{}    -> True
                    EventThreadDelay{}  -> True
                    _                   -> False
                )
                next

drainTQueue :: (MonadSTM m) => TQueue m a -> STM m [a]
drainTQueue tc = do
    msg <- tryReadTQueue tc
    case msg of
        Nothing -> pure []
        Just v  -> (v :) <$> drainTQueue tc

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Check the precondition
prec ::
    StateMachine model cmd sut resp ->
    model Symbolic ->
    cmd Symbolic ->
    Property
prec StateMachine{precondition} smodel scmd =
    let e ce = counterexample ("Precondition failed: " <> show ce) $ property False
     in case logic (precondition smodel scmd) of
            VFalse ce  -> e ce
            _otherwise -> property True

-- | Check the postcondition
post ::
    (Show a) =>
    StateMachine model cmd sut resp ->
    model Concrete ->
    cmd Concrete ->
    resp Concrete ->
    Bool ->
    SimTrace a ->
    Property
post StateMachine{postcondition} cmodel ccmd cresp wasRace trace =
    case logic (postcondition cmodel ccmd cresp) of
        VFalse ce ->
            counterexample (ppTrace trace)
                $ counterexample
                    ( (if wasRace then ("Race found! " <>) else id)
                        "Postcondition failed: "
                        <> show ce
                    )
                $ property False
        _otherwise -> property True

-- | Check the invariant
inv ::
    (Show a) =>
    StateMachine model cmd sut resp ->
    model Concrete ->
    Bool ->
    SimTrace a ->
    Property
inv StateMachine{invariant} cmodel wasRace trace =
    case logic (fromMaybe (const Top) invariant cmodel) of
        VFalse ce ->
            counterexample (ppTrace trace)
                $ counterexample
                    ( (if wasRace then ("Race found! " <>) else id)
                        "Invariant broken: "
                        <> show ce
                    )
                $ property False
        _otherwise -> property True

mockCheck ::
    ( Foldable t
    , Rank2.Foldable resp
    , Show (cmd Concrete)
    , Show (t a)
    , Show (resp Concrete)
    ) =>
    cmd Concrete ->
    resp Concrete ->
    t a ->
    (Property, [Dynamic])
mockCheck ccmd cresp vars =
    let cvars = Rank2.foldMap (\(Concrete x) -> [toDyn x]) cresp
     in if length vars /= length cvars
            then
                let err = mockSemanticsMismatchError (ppShow ccmd) (ppShow vars) (ppShow cresp) (ppShow cvars)
                 in (counterexample ("Mock semantics mismatch: " <> err) $ property False, cvars)
            else (property True, cvars)

mockSemanticsMismatchError :: String -> String -> String -> String -> String
mockSemanticsMismatchError cmd svars cresp cvars =
    unlines
        [ ""
        , "Mismatch between `mock` and `semantics`."
        , ""
        , "The definition of `mock` for the command:"
        , ""
        , "    "
        , cmd
        , ""
        , "returns the following references:"
        , ""
        , "    "
        , svars
        , ""
        , "while the response from `semantics`:"
        , ""
        , "    "
        , cresp
        , ""
        , "returns the following references:"
        , ""
        , "    "
        , cvars
        , ""
        , "Continuing to execute commands at this point could result in scope"
        , "errors, because we might have commands that use references (returned"
        , "by `mock`) that are not available (returned by `semantics`)."
        , ""
        ]
