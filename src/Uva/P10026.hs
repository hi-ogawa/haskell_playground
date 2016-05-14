module Uva.P10026 (spec) where

-- Problem: https://uva.onlinejudge.org/external/100/p10026.pdf

-- TODO: prove correctness of the algorithm

import Data.List (sortBy)
import Data.Ord (comparing)
import Test.Hspec

data Task = Task Int Int deriving (Eq)

instance Ord Task where
  compare (Task t0 f0) (Task t1 f1) = compare (t0 * f1) (t1 * f0)

solve :: [(Int, Int)] -> [Int]
solve = map fst . sortBy (comparing snd) . zip [1..] . map (uncurry Task)

spec :: Spec
spec =
  describe "solve" $ it "." $
    solve [(3, 4), (1, 1000), (2, 2), (5, 5)] `shouldBe` [2, 1, 3, 4]
