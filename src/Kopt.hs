{-# LANGUAGE TupleSections #-}
module Kopt (threeOpt, spec) where

import qualified Data.Vector as V
import Data.Function (fix)
import Data.List (find)

import Test.Hspec

-- assume complete graph is represented as adjacency matrix
type Matrix = V.Vector (V.Vector Double)
type Path = V.Vector Int

-- four pattern of 3 edge swapping
data Pattern = A | B | C | D
             deriving (Show, Eq)

threeOpt :: Matrix -> [Int]
threeOpt mtrx = do
  let numVs = V.length mtrx
      trpls = triples numVs
      dbls = doubles numVs
      initPath = V.fromList $ [0..(numVs - 1)] ++ [0]
  V.toList $ (`fix` initPath) $ \loop path ->
    let mybTrpl = find (goodTriple mtrx path) trpls
        mybDbl  = find (goodDouble mtrx path) dbls in
    case (mybTrpl, mybDbl) of
      (Just trpl, _) -> loop $ swapTriple path trpl
      (_, Just dbl)  -> loop $ swapDouble path dbl
      _              -> path

weight :: Matrix -> Path -> Int -> Int -> Double
weight mtrx path i i' = mtrx V.! (path V.! i) V.! (path V.! i')

goodTriple :: Matrix -> Path -> (Int, Int, Int, Pattern) -> Bool
goodTriple mtrx path trpl@(i, j, k, _) =
  let decr = sum $ map (uncurry (weight mtrx path)) [(i, i+1), (j, j+1), (k, k+1)]
      incr = sum $ map (uncurry (weight mtrx path)) (tripleEdgesIdcs trpl) in
  incr < decr

goodDouble :: Matrix -> Path -> (Int, Int) -> Bool
goodDouble mtrx path dbl@(i, j) =
  let decr = sum $ map (uncurry (weight mtrx path)) [(i, i+1), (j, j+1)]
      incr = sum $ map (uncurry (weight mtrx path)) (doubleEdgesIdcs dbl) in
  incr < decr

swapTriple :: Path -> (Int, Int, Int, Pattern) -> Path
swapTriple path trpl = V.fromList $ map (path V.!) $ triplePathIdcs (length path) trpl

swapDouble :: Path -> (Int, Int) -> Path
swapDouble path dbl = V.fromList $ map (path V.!) $ doublePathIdcs (length path) dbl

-- [] if n < 5
triples :: Int -> [(Int, Int, Int, Pattern)]
triples n = [ (i, j, k, p)
             | i <- [0..n-1]
             , j <- [i+2..n-1]
             , k <- [j+2..n-1]
             , p <- [A, B, C, D]
             , j - i >= 2
             , k - j >= 2
             ]

-- [] if n < 3
doubles :: Int -> [(Int, Int)]
doubles n = [ (i, j) | i <- [0..n-1], j <- [i+2..n-1], j - i >= 2 ]

tripleEdgesIdcs :: (Int, Int, Int, Pattern) -> [(Int, Int)]
tripleEdgesIdcs (i, j, k, p) =
  case p of
    A -> [(i, j  ), (i+1, k  ), (j+1, k+1)]
    B -> [(i, j+1), (j,   k+1), (k,   i+1)]
    C -> [(i, j+1), (i+1, k+1), (k,   j  )]
    D -> [(i, k  ), (k+1, j+1), (j,   k+1)]

triplePathIdcs :: Int -> (Int, Int, Int, Pattern) -> [Int]
triplePathIdcs n (i, j, k, p) =
  case p of
    A -> [0..i-1] ++ [i, j] ++ reverse [i+1..j-1] ++ reverse [j+1..k] ++ [k+1..n-1]
    B -> [0..i-1] ++ [i] ++ [j+1..k] ++ [i+1..j]                      ++ [k+1..n-1]
    C -> [0..i-1] ++ [i] ++ [j+1..k] ++ reverse [i+1..j]              ++ [k+1..n-1]
    D -> [0..i-1] ++ [i] ++ reverse [j+1..k] ++ [i+1..j]              ++ [k+1..n-1]

doubleEdgesIdcs :: (Int, Int) -> [(Int, Int)]
doubleEdgesIdcs (i, j) = [(i, j), (i+1, j+1)]

doublePathIdcs :: Int -> (Int, Int) -> [Int]
doublePathIdcs n (i, j) = [0..i] ++ reverse [i+1..j] ++ [j+1..n-1]


-------------
-- testing --

mkAdjMatrix :: [(Double, Double)] -> Matrix
mkAdjMatrix pts =
  let distance (x, y) (x', y') = sqrt $ (x - x') ^ 2 + (y - y') ^ 2 in
  V.fromList [ V.fromList [ distance p p' | p' <- pts] | p <- pts ]

spec :: Spec
spec =
  describe "threeOpt" $
    it "." $ do
      -- complete graph verteces on x,y cordinates
      --    +----------+
      --  4 | 1 4    7 |
      --  3 |       6  |
      --  2 |  23      |
      --  1 |          |
      --  0 |0     5  8|
      --    +----------+
      --     0123456789
      let pts = [ (0, 0) -- 0
                , (1, 4) -- 1
                , (2, 2) -- 2
                , (3, 2) -- 3
                , (3, 4) -- 4
                , (6, 0) -- 5
                , (7, 3) -- 6
                , (8, 4) -- 7
                , (9, 0) -- 8
                ]
      threeOpt (mkAdjMatrix pts) `shouldBe` [0,1,4,6,7,8,5,3,2,0]
