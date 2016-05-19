module LoopThings where

import Control.Arrow
import Control.Monad
import Data.IORef
import Control.Monad.Except

-- MOTIVATION:
--   realize imperative `while true break` with `do` notation in haskell
--    - it doesn't have to be "safe"
--    - syntactical similarity/clarity is important
--   comparison of
--    - loop (Control.Arrow)
--    - fix, mfix (Data.Function, Control.Monad.Fix)

-- TODO:
--   - find relation between loop and fix

-- Is while impossible to realize by loop from Control.Arrow ??
-- input  b
-- output c
--
-- x = p(b)
-- while f(x) {
--   x = g x
-- }
-- c = q(x)

loopMan :: (b -> d) -> (d -> c) -> (d -> Bool) -> (d -> d) -> (b -> c)
loopMan p q f g = loop h'
  where
    h (_b, _d) | f _d == True  = (undefined, g _d)
               | f _d == False = (q _d, undefined)
    h' (_b, _d) = h (undefined, p _b)

ex0 :: IO ()
ex0 = do
  let x = loopMan (+10) (8-) (<= 20) (+1) 0
  print x


-- while loop in monad
ex1 :: IO ()
ex1 = do
  x <- newIORef (0 :: Int)
  flip fix () $ \f _ -> do
    i <- readIORef x
    if i > 10
    then return ()
    else do
      modifyIORef x (+1)
      f ()
  print =<< readIORef x

  y <- newIORef (0 :: Int)
  fix $ \f -> do
    i <- readIORef y
    if i > 10
    then return ()
    else do
      modifyIORef y (+1)
      f
  print =<< readIORef y


-- while loop in pure context
ex2 :: IO ()
ex2 = do
  let x :: Int
      x = whileTrueWith 0 $ \i -> do
        when (i > 10) (throwError i)
        return (i + 1)
  print x

  let x' :: Int
      x' = while (<= 10) (+1) 0
  print x'

  let x'' :: Int
      x'' = flip fix 0 $ \f i ->
        if i > 10
        then i
        else f (i + 1)
  print x''

whileTrueWith :: a -> (a -> Either a a) -> a
whileTrueWith x = either id undefined . (flip iterateM_ x)
  where
    iterateM_ :: Monad m => (a -> m a) -> a -> m ()
    iterateM_ f = sequence_ . iterate (f =<<) . pure

while :: (a -> Bool) -> (a -> a) -> a -> a
while f g = head . dropWhile f . iterate g
