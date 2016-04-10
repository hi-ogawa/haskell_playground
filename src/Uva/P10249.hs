{-# LANGUAGE ScopedTypeVariables #-}
module P10249 where

import Test.Hspec

import Control.Arrow
import Data.Array
import Data.Array.IO -- TODO: try MVector
import Control.Monad

type CompM a = IO a


-- NOTE:
--  - `Graph` (Adjacency list) is for faster search
--  - residual network is calculated call from `Capacities` and `Flow`
type Graph = IOArray Int [Int]
type Capacities = IOArray (Int, Int) Int
type Flow = IOArray (Int, Int) Int


---------------
-- algorithm --

solve :: [Int] -> [Int] -> IO (Maybe [[Int]])
solve teams tables = do
  arrangements
  where
    arrangements = undefined

constructGraph :: [Int] -> [Int] -> CompM (Graph, Capacities, Int, Int, [Int], [Int])
constructGraph teams tables = do
  -- NOTE: Verteces Definition:
  --   source : 0
  --   sink   : 1
  --   teams  : 2 .. (n + 1)
  --   tables : (n + 2) .. (n + m + 1)
  let n = length teams
      m = length tables
  graph <- newListArray (0, n + m + 1) (repeat [])
  capacities <- newListArray ((0, 0), (n + m + 1, n + m + 1)) (repeat 0)
  -- source -> teams
  writeArray graph 0 [2..(n + 1)]
  forM_ [0] $ \i ->
    forM_ (zip [2..(n + 1)] teams) $ \(j, c) ->
      writeArray capacities (i, j) c
  -- teams -> tables
  forM_ [2..(n + 1)] $ \i ->
    forM_ [(n + 2)..(n + m + 1)] $ \j -> do
      modifyArray graph i (j:)
      writeArray capacities (i, j) 1
  -- teams -> sink
  forM_ (zip [(n + 2)..(n + m + 1)] tables) $ \(j, c) -> do
    writeArray graph j [1]
    writeArray capacities (j, 1) c
  return (graph, capacities, 0, 1, [2..(n+1)], [(n+2)..(n+m+1)])


-- NOTE: Algorithm Procedure
-- 1. initialize flow
-- 2. loop until (b) doesn't find such path
--   a. calculate residual network from flow
--   b. find shortest path following positive flow in the residual network
--   c. update flow with the positive flow path
-- 3. return flow
algoEdmondsKarp :: Int -> Int -> Graph -> Capacities -> CompM Flow
algoEdmondsKarp s t graph capacities = do
  initFlow <- mapArray (const 0) capacities
  undefined


---------------
-- utilities --

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray ar i f = do
  writeArray ar i . f =<< readArray ar i


-----------
-- specs --

spec :: Spec
spec = do
  describe "constructGraph" $ it "." $ do
    (g, c, _, _, _, _) <- constructGraph [4, 5, 3, 5] [3, 5, 2, 6, 4]
    g' <- freeze g
    c' <- freeze c
    g' `shouldBe` array (0,10) [(0,[2,3,4,5]),(1,[]),(2,[10,9,8,7,6]),(3,[10,9,8,7,6]),(4,[10,9,8,7,6]),(5,[10,9,8,7,6]),(6,[1]),(7,[1]),(8,[1]),(9,[1]),(10,[1])]
    c' `shouldBe` array ((0,0),(10,10)) [((0,0),0),((0,1),0),((0,2),4),((0,3),5),((0,4),3),((0,5),5),((0,6),0),((0,7),0),((0,8),0),((0,9),0),((0,10),0),((1,0),0),((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),((1,8),0),((1,9),0),((1,10),0),((2,0),0),((2,1),0),((2,2),0),((2,3),0),((2,4),0),((2,5),0),((2,6),1),((2,7),1),((2,8),1),((2,9),1),((2,10),1),((3,0),0),((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((3,6),1),((3,7),1),((3,8),1),((3,9),1),((3,10),1),((4,0),0),((4,1),0),((4,2),0),((4,3),0),((4,4),0),((4,5),0),((4,6),1),((4,7),1),((4,8),1),((4,9),1),((4,10),1),((5,0),0),((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),0),((5,6),1),((5,7),1),((5,8),1),((5,9),1),((5,10),1),((6,0),0),((6,1),3),((6,2),0),((6,3),0),((6,4),0),((6,5),0),((6,6),0),((6,7),0),((6,8),0),((6,9),0),((6,10),0),((7,0),0),((7,1),5),((7,2),0),((7,3),0),((7,4),0),((7,5),0),((7,6),0),((7,7),0),((7,8),0),((7,9),0),((7,10),0),((8,0),0),((8,1),2),((8,2),0),((8,3),0),((8,4),0),((8,5),0),((8,6),0),((8,7),0),((8,8),0),((8,9),0),((8,10),0),((9,0),0),((9,1),6),((9,2),0),((9,3),0),((9,4),0),((9,5),0),((9,6),0),((9,7),0),((9,8),0),((9,9),0),((9,10),0),((10,0),0),((10,1),4),((10,2),0),((10,3),0),((10,4),0),((10,5),0),((10,6),0),((10,7),0),((10,8),0),((10,9),0),((10,10),0)]
