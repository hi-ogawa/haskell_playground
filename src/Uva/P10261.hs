{-# LANGUAGE FlexibleContexts #-}
module Uva.P10261 (spec) where

-- Problem: https://uva.onlinejudge.org/external/102/10261.pdf

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Maybe
import Test.Hspec

data Position = P | S deriving (Eq)
type DPA = IOArray (Int, Int) (Maybe (Int, Int))

instance Show Position where
  show P = "port"
  show S = "starboard"

solve :: Int -> [Int] -> IO [Position]
solve l' cars = do
  let l = l' * 100
      n = length cars
      carsA = listArray (1, n) cars

  -- initialize and set boundary
  a <- newListArray ((0, 0), (n, l)) (repeat Nothing) :: IO DPA
  forM_ [0..n] $ \i ->
    writeArray a (i, 0) (Just undefined)

  -- recursive calculation
  forM_ [1..n] $ \i ->
    forM_ [1..l] $ \j -> do
      let prevs = [ (i - 1, j - (carsA ! i))
                  , (i - 1, j) ]
      forM_ prevs $ \prev -> do
        myb <- readArray' a prev
        case myb of
          Nothing -> return ()
          Just _ -> writeArray a (i, j) (Just prev)

  -- get answer
  final_ij <-
    ($ reverse (range ((0, 0), (n, l)))) $
      filterM (\ij@(i, j) -> do
        myb <- readArray a ij
        return $
          isJust myb &&
          sum (take i cars) - j <= l
      ) >=>
      return . head

  -- backtracking
  let backtrack (i, j) | j == 0 = return $ replicate i S
                       | otherwise = do
        Just (i', j') <- readArray a (i, j)
        (if j == j' then (S:) else (P:)) <$> backtrack (i', j')

  reverse <$> backtrack final_ij

  where
    readArray' :: DPA -> (Int, Int) -> IO (Maybe (Int, Int))
    readArray' ar idx = do
      bnds <- getBounds ar
      if inRange bnds idx
      then readArray ar idx
      else return Nothing


----------
-- spec --

spec :: Spec
spec =
  describe "solve" $ it "." $ do
    answer <- solve 50 [2500, 3000, 1000, 1000, 1500, 700, 800]
    answer `shouldBe` [S, P, P, P, S, S]
