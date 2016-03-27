{-# LANGUAGE OverloadedStrings #-}

module Ml.BackPropagation where

import Data.Function
import Data.List (partition)
import Data.Bits (xor)
import System.Random
import Numeric.LinearAlgebra hiding (range)
import Control.Monad.Writer
import Debug.Trace (trace)
import Control.Arrow ((***))

import Data.Array (range)

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- Things to Learn:
--   - use 3rd pary linear algebra library:
--     - http://dis.um.es/~alberto/hmatrix/hmatrix.html (choose this)
--     - https://wiki.haskell.org/Libraries_and_tools/Mathematics#Linear_algebra
--   - json for output data
--     - https://hackage.haskell.org/package/aeson
--   - chain rule: https://en.wikipedia.org/wiki/Chain_rule
--     - (f . g)' = (f' . g) * g'
--   - what brings "forward calculation" ?
--     - it comes from derivative of square error (not from multi-layer-ness)
--     - umm, is this really true?
--   - hypabolic function
--     - x^2 - y^2 = 1
--     - sinh(x) = (e^x - e^{-x})/2
--     - cosh(x) = (e^x + e^{-x})/2
--     - tanh(x) = sin(x)/conh(x)
--   - tanh enables normalization, not 1 / 1 + e^{-x}
--     - it needs to be f(-x) = -f(x)
--     - nah, it's not exactly true. but derivative can be considered same.
--     - so, I don't take normalization for `oneMiddleLayer`

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
  -- B.writeFile "resources/Ml/BackPropagation/points1.json" (encode (samples1 100))
  -- B.writeFile "resources/Ml/BackPropagation/lines_dumb0.json" (encode (uncurry noMiddleLayer (samples0 100)))
  -- B.writeFile "resources/Ml/BackPropagation/lines_dumb1.json" (encode (uncurry noMiddleLayer (samples1 100)))
  B.writeFile "resources/Ml/BackPropagation/lines0.json" (encode (uncurry oneMiddleLayer (samples0 100) 20))
  -- B.writeFile "resources/Ml/BackPropagation/lines1.json" (encode (uncurry oneMiddleLayer (samples1 100) 3))


-----------------------------------------------------------------------
-- single layer neural network (aka. perceptron w/ sigmoid function) --

noMiddleLayer :: [(R, R)] -> [(R, R)] -> [Vector R {-R^3-}]
noMiddleLayer xs xs' = normalizedNoMiddleLayer (nXs ++ nXs')
  where
    nXs = map (\(x0, x1) -> vector [x0, x1, 1]) xs
    nXs' = map (\(x0, x1) -> vector [-x0, -x1, -1]) xs'

-- normalization works since 1 - f(x) == f(-x)
normalizedNoMiddleLayer :: [Vector R] -> [Vector R]
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

oneMiddleLayer :: [(R, R)] -> [(R, R)] -> Int -> [([[R]], [R])]
oneMiddleLayer xs xs' m = map (toLists *** toList) $ oneMiddleLayer' (xys ++ xys') m
  where
    -- since Image(f) \subseteq [0, 1]
    xys = map (\(x0, x1) -> (vector [x0, x1, 1], 1)) xs
    xys' = map (\(x0, x1) -> (vector [x0, x1, 1], 0)) xs'

{-
[input layer]                    [middle layer]                 [output layer]

     x  |--(ap. w)-(p.w. ap. f)-->  g w x  |--(ap. w')-(ap. f)--> h w' (g w x)

   \in X                           \in X'                         \in [0, 1]
-}

-- NOTE: only for syntactical readability
type X = Vector R  {-R^3-}
type X' = Vector R {-R^m-}
type W = Matrix R  {-R^m * R^3-}
type W' = Vector R {-R^m-}

oneMiddleLayer' :: [(X, R)] -> Int -> [(W, W')]
oneMiddleLayer' xys m = execWriter $ loop n init_w_w'
  where
    n = 200
    r = 0.5
    eps = 0.5
    -- TODO: see why initial data matters
    -- init_w_w' = ((m><3) [(0.1),(0.1)..], vector (take m [(0.1),(0.1)..]))
    init_w_w' = ((m><3) [1..], vector (take m [1..]))
    -- init_w_w' = ((m><3) (randomRs (0, 0.001) (mkStdGen 10)), vector (take m (randomRs (0, 0.001) (mkStdGen 20))))
    loop :: Int -> (W, W') -> Writer [(W, W')] ()
    loop n (w, w') = do
      tell [(w, w')]
      unless ((debug (_E w w')) <= eps || n <= 0) $ do
        let (dw, dw') = grad_E w w'
        loop (n-1) (w - (scalar r) * dw, w' - (scalar r) * dw')

    g :: W -> X -> X'
    g w x = cmap f (w #> x)
    h :: W' -> X' -> R
    h w' x' = f (w' <.> x')

    _E :: W -> W' -> R
    _E w w' = xys & sum . map (_e w w')
    _e :: W -> W' -> (X, R) -> R
    _e w w' (x, y) = (1/2) * (h w' (g w x) - y)^2

    grad_E :: W -> W' -> (W, W')
    grad_E w w' = xys & foldl pAdd ((m><3) [0,0..], vector (take m [0,0..])) . map (grad_e w w')
    grad_e :: W -> W' -> (X, R) -> (W, W')
    grad_e w w' (x, y) = let
      -- discent of w needs to be computed depends on next w'?
      d_e_d_w' j =     (h w' (g w x) - y) * (f' (w' <.> g w x)) * (g w x ! j)
      d_e_d_w (j, i) = (h w' (g w x) - y) * (f' (w' <.> g w x)) * (w' ! j) * (f' (g w x ! j)) * (x ! i) in
      (
        (m><3) . map d_e_d_w $ range ((0, 0), (m-1, 2)),
        vector . map d_e_d_w' $ range (0, m-1)
      )

-------------------------
-- activation function --
--  https://en.wikipedia.org/wiki/Sigmoid_function

-- f :: R -> R
-- f x = (exp(x) - exp(-x)) / (exp(x) + exp(-x))

-- f' :: R -> R
-- f' x = 1 - f(x)^2

f :: R -> R
f x = 1 / (1 + exp (-x) )

f' :: R -> R
f' x = f(x) * (1 - f(x))

pAdd :: Num a => Num b => (a, b) -> (a, b) -> (a, b)
pAdd (a, b) (a', b') = (a + a', b + b')

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


-- TODO: define custom ToJSON instance
-- instance ToJSON (Matrix a) where
--   toJSON :: Num a => Value
--   toJSON mat = object ["size" .= size mat]
--   toJSON mat = undefined

-----------
-- debug --

debug :: Show a => a -> a
debug x = trace (show x) x

debugM :: Show a => Monad m => a -> m a
debugM x = trace (show x) (return x)
