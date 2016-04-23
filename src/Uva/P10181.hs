{-# LANGUAGE TupleSections #-}
module Uva.P10181 (spec) where

import Test.Hspec
import Control.Arrow ((>>>))
import Control.Exception (assert)
import Data.Array
import Data.Function (fix)
import qualified Data.Heap as H
import Data.List (find, findIndex)
import Data.Maybe (fromJust, catMaybes)

type Puzzle = Array (Int, Int) Int
data HpElem = HpElem Puzzle [Move] Int
type Hp = H.Heap HpElem
data Move = R | L | U | D deriving (Show, Eq)

instance Eq HpElem where
  (HpElem _ _ i) == (HpElem _ _ j) = i == j

instance Ord HpElem where
  (HpElem _ _ i) <= (HpElem _ _ j) = i <= j


solve :: [Int] -> Maybe [Move]
solve initPositions =
  let puzzle = listArray ((0, 0), (3, 3)) initPositions :: Puzzle in
  if solvable puzzle
  then Just $ aStar puzzle
  else Nothing


-- [A* algorithm with consistent heuristics]
-- - initialize min-heap
-- - loop by
--   - find minElem from heap
--   - if h(minElem) == 0, loop is done
--   - otherwise, put neighbors of minElem into heap with stacking Move history for each
aStar :: Puzzle -> [Move]
aStar puzzle =
  let initHp = H.singleton (HpElem puzzle [] (h puzzle)) :: Hp in
  reverse . (flip fix) initHp $ \loop hp ->
    case H.viewMin hp of
      Nothing -> assert False undefined
      Just (HpElem p mvs c, hp') ->
        if h p == 0
        then mvs
        else
          loop . (flip inserts hp') . map (\(p', m) -> HpElem p' (m:mvs) (c+1)) $ nexts p

nexts :: Puzzle -> [(Puzzle, Move)]
nexts puzzle =
  let (i, j) = fst . fromJust . find ((0 ==) . snd) . assocs $ puzzle
      next :: Move -> Maybe Puzzle
      next R | j /= 3 = Just $ swap puzzle (i, j) (i, j + 1)
      next L | j /= 0 = Just $ swap puzzle (i, j) (i, j - 1)
      next U | i /= 0 = Just $ swap puzzle (i, j) (i - 1, j)
      next D | i /= 3 = Just $ swap puzzle (i, j) (i + 1, j)
      next _ = Nothing in
  catMaybes . map (\m -> (,m) `fmap` next m) $ [R, L, U, D]
  where
    swap :: Ix i => Array i e -> i -> i -> Array i e
    swap ar p q = ar // [(p, ar ! q), (q, ar ! p)]

inserts :: Ord a => [a] -> H.Heap a -> H.Heap a
inserts = foldl (\f e -> f . H.insert e) id

-- admissible, consistent heuristics #(misplaced tiles)
h :: Puzzle -> Int
h = length . filter (uncurry (/=)) . zip [1..15] . elems


-----------------
-- solvability --

solvable :: Puzzle -> Bool
solvable puzzle = do
  let (i, j) = fst . fromJust . find ((0 ==) . snd) . assocs $ puzzle
      ls = elems puzzle
  even $ (3 - i) + (3 - j) + permutationParity ([1..15] ++ [0]) ls

permutationParity :: [Int] -> [Int] -> Int
permutationParity ls ls' =
  let n = length ls
      ar = listArray (0, n - 1) ls in
  ($ [(i, j) | i <- [0..(n - 1)], j <- [(i + 1)..(n - 1)]]) $ (
     filter $ \(x, y) ->
       (fromJust $ findIndex ((ar!x) ==) ls') > (fromJust $ findIndex ((ar!y) ==) ls')
     ) >>>
     length


----------
-- spec --

spec :: Spec
spec = do
  describe "permutationParity" $ do
    it "." $ do
      permutationParity [0, 1, 2, 3] [3, 1, 2, 0] `shouldBe` 5
  describe "solve" $ do
    it "." $ do
      solve [2,3,4,0,1,5,7,8,9,6,10,12,13,14,11,15] `shouldBe` Just [L, L, L, D, R, D, R, D, R]
    it "." $ do
      solve [13,1,2,4,5,0,3,7,9,6,10,12,15,8,11,14] `shouldBe` Nothing
