module Ml.Perceptron where

import System.Random
import Data.List (partition, intercalate)
import Numeric (showFFloat)
import Control.Monad.Writer

-- TODO:
--   - implement two versions
--     - offline
--     - online
--   - convergence theorem of linearly seperable instances and time complexity
--   - gradient descent terminology:
--     - learning rate - step size (must be 0..1 ?)
--     - standard - offline - batch
--     - online - stochatic (mathematical derivation of this method)


------------------------
-- Problem Definition --

-- GIVEN:
--   - class a: {..., x_i, ...}  \subseteq X
--   - class b: {..., x'_i, ...} \subseteq X'
-- FIND linear function f: R^2 -> R
-- s.t.
--   for all x  \in X.  f(x)  >= 0        -- (a)
--   for all x' \in X'. f(x') <  0        -- (a')

-- notes:
--   - we deal with only the special case where
--     - input is two dimentional
--     - 2 categories classification
--   - represent linear function f as coefficients (w0, w1) and bias constant (w2):
--      f : (u, v) |-> w0*u + w1*v + w2
--   - we employ normalization for calculation simplicity: e.g.
--      - extend (u, v) to (u, v, 1) to deal with `w2` nicely
--      - negate the elements in the second class X'

----------
-- main --

{- usage:
 $ runHaskell src/Ml/Perceptron.hs
 $ python3 resources/Ml/Perceptron/plot.py
-}

main :: IO ()
main = do
  writeFile "resources/Ml/Perceptron/points.dat" (formatSamples samples)
  writeFile "resources/Ml/Perceptron/lines.dat" (formatLines $ uncurry offline samples)


type R_2 = (Double, Double)
type R_3 = (Double, Double, Double)

---------------
-- algorithm --

rate, eps :: Double
rate = 0.001
eps = 0.00001

-- returns each parameters of gradient descent steps
offline :: [R_2] -> [R_2] -> [R_3]
offline xs xs' = offlineNormalized (nXs ++ nXs')
  where
    nXs  = map (\(u, v) -> (u, v, 1)) xs
    nXs' = map (\(u, v) -> (-u, -v, -1)) xs'

offlineNormalized :: [R_3] -> [R_3]
offlineNormalized xs = execWriter $ loop (0.1, 0.1, 0.1)
  where
    loop :: R_3 -> Writer [R_3] ()
    loop p | _E p <= eps = tell [p] >> return ()
           | otherwise = tell [p] >> loop (p `minus` (rate `times` grad_E p))
    _E :: R_3 -> Double
    _E p = sum . map (negate . (p `dot`)) . filter ((0 >) . (p `dot`)) $ xs
    grad_E :: R_3 -> R_3
    grad_E p = (dE0 p, dE1 p, dE2 p)
    dE0, dE1, dE2 :: R_3 -> Double
    dE0 p = sum . map (negate . \(u, _, _) -> u) . filter ((0 >) . (p `dot`)) $ xs
    dE1 p = sum . map (negate . \(_, v, _) -> v) . filter ((0 >) . (p `dot`)) $ xs
    dE2 p = sum . map (negate . \(_, _, s) -> s) . filter ((0 >) . (p `dot`)) $ xs


-- vector calculation

dot :: R_3 -> R_3 -> Double
dot (w0, w1, w2) (u, v, s) = w0*u + w1*v + w2*s

minus :: R_3 -> R_3 -> R_3
minus (w0, w1, w2) (w0', w1', w2') = (w0 - w0', w1 - w1', w2 - w2')

times :: Double -> R_3 -> R_3
times c (w0, w1, w2) = (c*w0, c*w1, c*w2)

-- linearly separable samples

samples :: ([R_2], [R_2])
samples = partition (\(u, v) -> lineFunc u >= v) . take n $ xs
  where
    n = 100
    g = mkStdGen 0
    xs :: [R_2]
    xs = fix (\f ls -> (ls !! 0, ls !! 1):(f (drop 2 ls))) $ randomRs (-1, 1) g
    lineFunc u = 0.8 * u + 0.4

formatSamples :: ([R_2], [R_2]) -> String
formatSamples samples = dat
  where
    dat = format (fst samples) ++ "\n\n" ++ format (snd samples)
    format :: [R_2] -> String
    format = unlines . map (\(u, v) -> showFloat u ++ " " ++ showFloat v)

formatLines :: [R_3] -> String
formatLines ls =
  unlines . map (\(x, y, z) -> intercalate " " . map showFloat $ [x, y, z]) $ ls

showFloat :: Double -> String
showFloat d = ($ "") $ showFFloat (Just 3) d
