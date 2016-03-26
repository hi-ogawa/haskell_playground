{-# LANGUAGE OverloadedStrings #-}

module Ml.BackPropagation where

import Data.Function
import Data.List (partition)
import Data.Bits (xor)
import System.Random
import Numeric.LinearAlgebra
import Control.Monad.Writer

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- Things to Learn:
--   - use 3rd pary linear algebra library:
--     - http://dis.um.es/~alberto/hmatrix/hmatrix.html (choose this)
--     - https://wiki.haskell.org/Libraries_and_tools/Mathematics#Linear_algebra
--   - json for output data to python
--     - https://hackage.haskell.org/package/aeson
--   - chain rule: https://en.wikipedia.org/wiki/Chain_rule
--     - (f . g)' = (f' . g) * g'

-- TODO:
--   - implement back propagation algorithm for single hidden layer neural network
--     - how do we define two class values?
--       - it depends on activation function: 0, 1 \in R or -1, 1 \in R
--     - squared error
--     - justify algorithm (at least intuitively)
--       - time complexity
--       - convergence

main :: IO ()
main = do
  B.writeFile "resources/Ml/BackPropagation/points0.json" (encode (samples0 100))
  B.writeFile "resources/Ml/BackPropagation/lines0.json" (encode (uncurry noMiddleLayer (samples0 100)))
  B.writeFile "resources/Ml/BackPropagation/points1.json" (encode (samples1 100))
  B.writeFile "resources/Ml/BackPropagation/lines1.json" (encode (uncurry noMiddleLayer (samples1 100)))


-----------------------------------------------------------------------
-- single layer neural network (aka. perceptron w/ sigmoid function) --

noMiddleLayer :: [(R, R)] -> [(R, R)] -> [Vector R {-R^3-}]
noMiddleLayer xs xs' = normalizedNoMiddleLayer (nXs ++ nXs')
  where
    nXs = map (\(x0, x1) -> vector [x0, x1, 1]) xs
    nXs' = map (\(x0, x1) -> vector [-x0, -x1, -1]) xs'

-- normalization works since 1 - f(x) == f(-x)
normalizedNoMiddleLayer :: [Vector R {-R^3-}] -> [Vector R {-R^3-}]
normalizedNoMiddleLayer xs = execWriter $ loop 100 (vector [1, 1, 1])
  where
    rate = 0.01
    eps = 0.5
    loop :: Int -> Vector R -> Writer [Vector R] ()
    loop n w = do
      tell [w]
      unless (_E w <= eps || n <= 0) $ loop (n-1) (w - (scalar rate * grad_E w))
    _E :: Vector R -> R
    _E w = xs & sum . map (\x -> f (w <.> x))
    grad_E :: Vector R -> Vector R
    grad_E w = xs & sum . map (\x -> scalar (f' (w <.> x)) * x)


--------------------------------
-- multi layer neural network --
oneMiddleLayer :: [Vector R] -> [Vector R] -> Int -> ()
oneMiddleLayer xs xs' m = undefined


-------------------------
-- activation function --
--  https://en.wikipedia.org/wiki/Sigmoid_function

f :: R -> R
f x = 1 / (1 + exp (-x) )

f' :: R -> R
f' x = f(x) * (1 - f(x))


-------------
-- samples --

randomR2s :: [(R, R)]
randomR2s = fix (\f ls -> (ls !! 0, ls !! 1):(f (drop 2 ls))) $ randomRs (-1, 1) (mkStdGen 0)

samples0 :: Int -> ([(R, R)], [(R, R)])
samples0 n = partition boundary . take n $ randomR2s
  where
    boundary (x, y) = (1.5 * x + 0.3 < y) `xor` (-2 * x + 0.3 < y)

samples1 :: Int -> ([(R, R)], [(R, R)])
samples1 n = partition boundary . take n $ randomR2s
  where
    boundary (x, y) = (x - 1) ^ 2 + y ^ 2 < 1
