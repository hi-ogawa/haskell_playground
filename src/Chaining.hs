module Chaining where

-- Motivation:
--  realize collection processing interface by chaining
--  like in ruby, scala, javascript (lodash/underscore)

import Control.Arrow ((>>>))
import Control.Monad

-- pure case
ex0 :: IO ()
ex0 = do
  let x = ($ [0..9]) $ (
        map $ \j ->
          j^2
        ) >>> (
        filter $ \v ->
          even v
        ) >>> (
        flip foldl 0 $ \acc v ->
          acc + v
        )
  print x

-- monad case
ex1 :: IO ()
ex1 = do
  x <- ($ [0..9]) $ (
    mapM $ \j -> do
      return (j^2)
    ) >=> (
    filterM $ \v -> do
      return (even v)
    ) >=> (
    flip foldM 0 $ \acc v ->
      return (acc + v)
    )
  print x
