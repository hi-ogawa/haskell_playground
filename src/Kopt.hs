{-# LANGUAGE TupleSections #-}
module Kopt (threeOpt, spec) where

-- import Control.Arrow ((>>>))
import Control.Monad
import Data.Array.IO -- TODO: using mutable array for swap operation doesn't make sense
import qualified Data.Vector as V
import Data.Function (fix)
import Data.Maybe (listToMaybe)

import Test.Hspec

-- assume complete graph is represented as adjacency matrix
type Matrix = V.Vector (V.Vector Double)
type Path = IOUArray Int Int

-- four pattern of 3 edge swapping
data Pattern = A | B | C | D
             deriving (Show, Eq)

threeOpt :: Matrix -> IO [Int]
threeOpt mtrx = do
  let numVs = V.length mtrx
      trpls = triples numVs
      dbls = doubles numVs
  path <- newListArray (0, numVs) ([0..(numVs - 1)] ++ [0]) :: IO Path
  fix $ \loop -> do
    mybTrpl <- findM (goodTriple mtrx path) trpls
    case mybTrpl of
      Just trpl -> swapTriple path trpl >> loop
      Nothing -> do
        mybDbl <- findM (goodDouble mtrx path) dbls
        case mybDbl of
          Just dbl -> swapDouble path dbl >> loop
          Nothing -> return ()
  getElems path

weight :: Matrix -> Path -> Int -> Int -> IO Double
weight mtrx path pathIdxOrig pathIdxDest = do
  vOrig <- readArray path pathIdxOrig
  vDest <- readArray path pathIdxDest
  return $ mtrx V.! vOrig V.! vDest

goodTriple :: Matrix -> Path -> (Int, Int, Int, Pattern) -> IO Bool
goodTriple mtrx path trpl@(i, j, k, _) = do
  prevWeight <- sum <$> mapM (uncurry (weight mtrx path)) [(i, i+1), (j, j+1), (k, k+1)]
  nextWeight <- sum <$> mapM (uncurry (weight mtrx path)) (tripleEdgesIdcs trpl)
  return $ nextWeight < prevWeight

goodDouble :: Matrix -> Path -> (Int, Int) -> IO Bool
goodDouble mtrx path dbl@(i, j) = do
  prevWeight <- sum <$> mapM (uncurry (weight mtrx path)) [(i, i+1), (j, j+1)]
  nextWeight <- sum <$> mapM (uncurry (weight mtrx path)) (doubleEdgesIdcs dbl)
  return $ nextWeight < prevWeight

swapTriple :: Path -> (Int, Int, Int, Pattern) -> IO ()
swapTriple path trpl =
  ($ triplePathIdcs trpl) $
    mapM (readArray path) >=>
    return . zip [0..] >=>
    mapM_ (\(i, v) -> writeArray path i v)

swapDouble :: Path -> (Int, Int) -> IO ()
swapDouble path dbl =
  ($ doublePathIdcs dbl) $
    mapM (readArray path) >=>
    return . zip [0..] >=>
    mapM_ (\(i, v) -> writeArray path i v)

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

triplePathIdcs :: (Int, Int, Int, Pattern) -> [Int]
triplePathIdcs (i, j, k, p) =
  case p of
    A -> [i, j] ++ (reverse [i+1..j-1]) ++ (reverse [j+1..k]) ++ [k+1]
    B -> [i] ++ [j+1..k] ++ [i+1..j] ++ [k+1]
    C -> [i] ++ [j+1..k] ++ (reverse [i+1..j]) ++ [k+1]
    D -> [i] ++ (reverse [j+1..k]) ++ [i+1..j] ++ [k+1]

doubleEdgesIdcs :: (Int, Int) -> [(Int, Int)]
doubleEdgesIdcs (i, j) = [(i, j), (i+1, j+1)]

doublePathIdcs :: (Int, Int) -> [Int]
doublePathIdcs (i, j) = [i] ++ (reverse [i+1..j]) ++ [j+1]

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f ls = listToMaybe <$> filterM f ls


-------------
-- testing --

mkAdjMatrix :: [(Double, Double)] -> Matrix
mkAdjMatrix pts =
  let distance (x, y) (x', y') = sqrt $ (x - x') ^ 2 + (y - y') ^ 2 in
  V.fromList [ V.fromList [ distance p p' | p' <- pts] | p <- pts ]

spec :: Spec
spec = do
  describe "threeOpt" $ do
    it "." $ do
      let pts = [ (0, 0)
                , (1, 4)
                , (2, 2)
                , (3, 2)
                , (3, 4)
                , (6, 0)
                , (7, 3)
                , (8, 4)
                , (9, 0)
                ]
      threeOpt (mkAdjMatrix pts) >>= (`shouldBe` [0,1,4,6,7,8,5,3,2,0])
