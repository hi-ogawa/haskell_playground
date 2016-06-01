module Debugger where

import Debug.Trace (trace)

debug :: Show b => (a -> b) -> a -> a
debug f x = trace (show (f x)) x

debugM :: (Monad m, Show b) => (a -> b) -> m a -> m a
debugM f mx = do
  x <- mx
  return $ trace (show (f x)) x
