{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Uva.P10051 (main, spec) where

import Test.Hspec
import Control.Monad
import Data.Array
import Data.Array.IO
import Data.List (maximumBy, intersperse)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.Parsec hiding (parse)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (natural)

main :: IO ()
main = do
  putStr =<< mainS =<< getContents

mainS :: String -> IO String
mainS = return . format <=< mapM solve . parse

parse ::  String -> [[[Color]]]
parse = either undefined id . runParser parser () ""
  where
    parser :: Parsec String () [[[Color]]]
    parser = do
      numCubes <- number
      if numCubes == 0
        then return []
        else do
        cubesColors <- count numCubes $ count 6 number
        (cubesColors:) `fmap` parser
    number :: Parsec String () Int
    number = fromInteger `fmap` natural haskell

format :: [[(Int, Face)]] -> String
format = join . intersperse "\n" . map showCase . zip ([1..] :: [Int])
  where
    showCase (iCase, stack) =
      "Case #" ++ show iCase ++ "\n" ++
      show (length stack) ++ "\n" ++
      showStack stack
    showStack = join . map (\(i, f) -> show i ++ " " ++ showFace f ++ "\n")
    showFace 0 = "front"
    showFace 1 = "back"
    showFace 2 = "left"
    showFace 3 = "right"
    showFace 4 = "top"
    showFace 5 = "bottom"
    showFace _ = undefined

---------------
-- algorithm --

type Face = Int
type Color = Int
type Cube = [Color]
type Cubes = Array Int Cube
-- (i, j) |-> (current maximum height, parent pointer, bottom face)
type DPA = IOArray (Color, Int) (Maybe (Int, (Color, Int), Face))

cMin, cMax :: Color
cMin = 1
cMax = 100

faces :: [Face]
faces = [0..5]

-- 0: front, 1: back, 2: left, 3: right, 4: top, 5: bottom
op :: Face -> Face
op 0 = 1
op 1 = 0
op 2 = 3
op 3 = 2
op 4 = 5
op 5 = 4
op _ = undefined

solve :: [[Color]]  -> IO [(Int, Face)]
solve cubesColors = do
  let n = length cubesColors
      cubes = listArray (1, n) cubesColors :: Cubes
  a <- newListArray ((cMin, 0), (cMax, n)) (repeat Nothing) :: IO DPA

  -- run DP
  forM_ [cMin..cMax] $ \i -> writeArray a (i, 0) (Just (0, undefined, undefined))
  forM_ [1..n] $ \j ->
    forM_ faces $ \f -> do
      let i  = (cubes ! j) !! f
          i' = (cubes ! j) !! (op f)
      (mx, k) <- ($ [0..(j-1)]) $ (
        mapMaybeM $ \k -> do
          myb <- readArray a (i', k)
          return $ (\(h, _, _) -> (h, k)) `fmap` myb
        ) >=> (
        return . maximumBy (comparing fst)
        )
      prev_a_i_j <- readArray a (i, j)
      case prev_a_i_j of
        Just (h, _, _) | h > mx + 1 -> return ()
        _ -> writeArray a (i, j) (Just (mx + 1, (i', k), f))

  -- find answer
  last_ij <- ($ [(i, j) | i <- [cMin..cMax], j <- [1..n]]) $ (
    mapMaybeM $ \(i, j) -> do
       myb <- readArray a (i, j)
       return $ (\(h, _, _) -> (h, (i, j))) `fmap` myb
    ) >=> (
    return . snd . maximumBy (comparing fst)
    )

  -- backtracking
  let backtrack (i, j) | j == 0 = return []
                       | otherwise = do
        Just (_, next_ij, f) <- readArray a (i, j)
        ((j, op f):) `fmap` backtrack next_ij

  reverse `fmap` backtrack last_ij


mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = mapM f >=> return . catMaybes


----------
-- spec --

spec :: Spec
spec = do
  describe "mainReturn" $ do
    it "." $ do
      input <- readFile "./resources/UVA10051.input"
      output <- mainS input
      expectation <- readFile "./resources/UVA10051.alt.output"
      output `shouldBe` expectation
  describe "solve" $ do
    it "." $ do
      answer <- solve [
        [1, 2, 2, 2, 1{-t-}, 2{-b-}],
        [3, 3, 3, 3, 3, 3],
        [3{-b-}, 2{-t-}, 1, 1, 1, 1]]
      answer `shouldBe` [(1, 4), (3, 1)]
    it "." $ do
      answer <- solve [
        [1, 5, 10, 3, 6{-b-}, 5{-t-}],
        [2{-b-}, 6{-t-}, 7, 3, 6, 9],
        [5, 7, 3{-b-}, 2{-t-}, 1, 9],
        [1, 3, 3{-t-}, 5{-b-}, 8, 10],
        [6, 6, 2, 2, 4, 4],
        [1, 2, 3, 4, 5, 6],
        [10, 9, 8, 7, 6{-b-}, 5{-t-}],
        [6{-t-}, 1{-b-}, 2, 3, 4, 7],
        [1, 2, 3, 3, 2{-b-}, 1{-t-}],
        [3, 2, 1, 1, 2{-t-}, 3{-b-}]]
      answer `shouldBe` [(1,5),(2,1),(3,3),(4,2),(7,5),(8,0),(9,5),(10,4)]
