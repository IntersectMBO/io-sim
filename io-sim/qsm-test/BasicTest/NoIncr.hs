-- |

module BasicTest.NoIncr where

import BasicTest.Template
import Control.Concurrent.Class.MonadSTM
import Test.QuickCheck
import Test.StateMachine.IOSim

{-------------------------------------------------------------------------------
  SUT
-------------------------------------------------------------------------------}

newtype AtomicCounter m = AtomicCounter (TVar m Int)

newSUT :: MonadLabelledSTM m => m (AtomicCounter m)
newSUT = do
    ref <- atomically $ do
      tv <- newTVar 0
      labelTVar tv "TheCounter"
      pure tv
    return (AtomicCounter ref)

incr :: MonadSTM m => AtomicCounter m -> m ()
incr (AtomicCounter ref) =
  atomically $ do
    i <- readTVar ref
    -- Deliberate bug, no increment
    writeTVar ref i

get :: MonadSTM m => AtomicCounter m -> m Int
get (AtomicCounter ref) = readTVarIO ref

semantics :: MonadSTM m => AtomicCounter m -> Cmd Concrete -> m (Resp Concrete)
semantics sut Incr = do
  incr sut *> pure Void
semantics sut Get = do
  GetR <$> get sut

transition :: Model r -> Cmd r -> Resp r -> Model r
transition (Model m) Incr _ = Model (m + 1)
transition m Get _          = m

sm :: StateMachine Model Cmd AtomicCounter Resp
sm = StateMachine initModel transition precondition postcondition Nothing
     generator shrinker newSUT semantics mock

prop_sequential :: Property
prop_sequential = forAllCommands sm Nothing $ runSequential sm
