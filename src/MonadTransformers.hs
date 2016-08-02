module MonadTransformers where

import Control.Monad.Except
import Control.Monad.State

type ES e s a = ExceptT e (State s) a
type SE e s a = StateT s (Except e) a

runES :: ES e s a -> s -> (Either e a, s)
runES m s = runState (runExceptT m) s

runSE :: SE e s a -> s -> Either e (a, s)
runSE m s = runExcept $ runStateT m s
