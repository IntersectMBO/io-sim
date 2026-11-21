-- |

module BasicTest.InternalFork where

import BasicTest.Template
import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadAsync
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

incr :: MonadAsync m => AtomicCounter m -> m ()
incr (AtomicCounter ref) = do
   t1 <- async f
   t2 <- async f
   wait t1 >> wait t2
  where f = do
          i <- readTVarIO ref
          atomically $ writeTVar ref (i + 1)

get :: (MonadSTM m) => AtomicCounter m -> m Int
get (AtomicCounter ref) = readTVarIO ref

semantics :: MonadAsync m => AtomicCounter m -> Cmd Concrete -> m (Resp Concrete)
semantics sut Incr = do
  incr sut *> pure Void
semantics sut Get = do
  GetR <$> get sut

transition :: Model r -> Cmd r -> Resp r -> Model r
-- each increment will increment by 2
transition (Model m) Incr _ = Model (m + 2)
transition m Get _          = m

sm :: StateMachine Model Cmd AtomicCounter Resp
sm = StateMachine initModel transition precondition postcondition Nothing
     generator shrinker newSUT semantics mock

prop_sequential :: Property
prop_sequential = forAllCommands sm Nothing $ runSequential sm

prop_sequential' :: Property
prop_sequential' = forAllCommands sm Nothing $ runSequentialPOR sm

prop_parallel :: Property
prop_parallel = forAllParallelCommands sm Nothing $ runParallel sm
