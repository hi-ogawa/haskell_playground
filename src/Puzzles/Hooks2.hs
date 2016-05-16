{-# LANGUAGE TupleSections #-}
module Puzzles.Hooks2 (main, spec) where

-- Problem: https://www.janestreet.com/puzzles/hooks-2/

-- Output of this program:
--   $ time cabal exec main -- hooks2
--   840
--   1600
--   2136
--   540
--   50
--   9
--   1
--   1
--   1
--   Answer is: 17418240
--
--   real	0m42.151s
--   user	0m41.771s
--   sys	0m0.330s


--------------
-- strategy --

-- 1. find hooks satisfying sudoku-like condition
--   - #(possible positions of hooks) is only 4^8 == 2^16 ~= 64000
--   - for each hooks' position, by dumb calculation, #(possible positions of numbers) is
--       \prod_{i \in {2, .., 9}} (i*2-1)Ci = 16,407,885,372,638,760,000
--   - pruning strategy:
--     - (a) when hook is put, one row and one column is filled.
--           so, if the sum of row/column doesn't satisfy condition at that point, you can prune.
--     - (b) anytime sum of row/column already exceeds the condition.
--   - I think just pruning along the way works well.

-- 2. find largest product of numbers on squres where no two squares sharing same row or column
--   - #(possible choices of squares) is only 9! = 362880


---------------
-- algorithm --

import Control.Arrow ((>>>))
import qualified Data.Array as A
import Data.List (permutations)
import Test.Hspec

import Debug.Trace (trace)

debug :: Show b => (a -> b) -> a -> a
debug f x = trace (show (f x)) x

data HkShp = A | B | C | D deriving (Show,Eq)
--                    _     _
-- A:  _|  B: |_  C: |   D:  |
--
type Board = A.Array (Int, Int) Int
data Candidate = Candidate {
  cBoard :: Board,
  _cHks :: [HkShp]
  } deriving (Show,Eq)

-- NOTE:
--   - a bunch of list join can be bottle-neck (can be replaced with Data.Sequence)
--   - Data.Array can be replaced Data.Vector

main :: IO ()
main = do
  putStr "Answer is: "
  print solve

solve :: Int
solve = maximum . map largestProduct $ validBoards

goalR, goalC :: [Int]
goalR = [45, 44, 4, 48, 7, 14, 47, 43, 33]
goalC = [36, 5, 47, 35, 17, 30, 21, 49, 45]


-------
-- 1 --

validBoards :: [Board]
validBoards =
  let validCandidates =
        ($ reverse [1..9]) $ flip foldl [start] $ \candidates hkSize ->
          debug length . filter (not.invalid) . concatMap (nexts hkSize) $ candidates in
  map cBoard validCandidates
  where
    start :: Candidate
    start = Candidate (A.listArray ((0,0), (8,8)) (repeat 0)) []

nexts :: Int -> Candidate -> [Candidate]
nexts hkSize (Candidate board hkShps)
  | hkSize == 1 = [Candidate (board A.// [(leftTopOfFreeArea hkShps, 1)]) (hkShps ++ [A])]
  | otherwise   =
    (`concatMap` [A, B, C, D]) $ \hkShp ->
      let numberPositions = hookArea hkSize hkShps hkShp `comb` hkSize in
      (`map` numberPositions) $ \numberPosition ->
        Candidate (board A.// map (,hkSize) numberPosition) (hkShps ++ [hkShp])

hookArea :: Int -> [HkShp] -> HkShp -> [(Int, Int)]
hookArea hkSize hkShps hkShp =
  let (i, j) = leftTopOfFreeArea hkShps
      s = hkSize
      -- four verteces and edges around free square space
      vLT = (i    , j)
      vRT = (i    , j+s-1)
      vRB = (i+s-1, j+s-1)
      vLB = (i+s-1, j)
      eL = A.range ((i+1  , j    ), (i+s-2, j    ))
      eR = A.range ((i+1  , j+s-1), (i+s-2, j+s-1))
      eT = A.range ((i    , j+1  ), (i    , j+s-2))
      eB = A.range ((i+s-1, j+1  ), (i+s-1, j+s-2)) in
  case hkShp of
    A -> eB ++ eR ++ [vRT, vRB, vLB]
    B -> eL ++ eB ++ [vLT, vLB, vRB]
    C -> eL ++ eT ++ [vLB, vLT, vRT]
    D -> eT ++ eR ++ [vLT, vRT, vRB]

leftTopOfFreeArea :: [HkShp] -> (Int, Int)
leftTopOfFreeArea = foldl move (0, 0)
  where
    move :: (Int, Int) -> HkShp -> (Int, Int)
    move (i, j) hkShp = case hkShp of
      A -> (i,   j  )
      B -> (i,   j+1)
      C -> (i+1, j+1)
      D -> (i+1, j  )

comb :: [a] -> Int -> [[a]]
comb ~xs@(h:l) n | n == 0         = [[]]
                 | n >= length xs = [xs]
                 | otherwise      = map (h:) (comb l (n-1)) ++ comb l n

invalid :: Candidate -> Bool
invalid c@(Candidate b _) = invalid0 c || invalid1 b

-- prune (a) (assume `length hkShps >= 1`)
invalid0 :: Candidate -> Bool
invalid0 (Candidate board hkShps) =
  let (hkShps', [hkShp]) = splitAt (length hkShps - 1) hkShps
      (i, j) = leftTopOfFreeArea hkShps'
      s = 9 - length hkShps'
      (i', j') = case hkShp of
        A -> (i+s-1, j+s-1)
        B -> (i+s-1, j    )
        C -> (i,     j    )
        D -> (i,     j+s-1) in
  (sum (map ((board A.!) . (i',)) [0..8]) /= goalR !! i') ||
  (sum (map ((board A.!) . (,j')) [0..8]) /= goalC !! j')

-- prune (b)
invalid1 :: Board -> Bool
invalid1 board = or $
  (`map` [0..8]) (\i -> sum (map ((board A.!) . (i,)) [0..8]) > goalR !! i) ++
  (`map` [0..8]) (\j -> sum (map ((board A.!) . (,j)) [0..8]) > goalC !! j)


-------
-- 2 --

largestProduct :: Board -> Int
largestProduct board =
  let subsets = map (zip [0..8]) $ permutations [0..8] in
  ($ subsets) $
    map (product . map (board A.!)
    ) >>>
    maximum


----------
-- spec --

spec :: Spec
spec = do
  describe "comb" $ it "." $
    comb [0, 1, 2, 3] 2 `shouldBe` [[0,1],[0,2],[0,3],[1,2],[1,3],[2,3] :: [Int]]
  describe "leftTopOfFreeArea" $ it "." $
    leftTopOfFreeArea [A, B, C, D, A, B] `shouldBe` (2, 3)
  describe "hookArea" $ it "." $
    hookArea 3 [A, B, C, D, A, B] C `shouldBe` [(3,3),(2,4),(4,3),(2,3),(2,5)]
