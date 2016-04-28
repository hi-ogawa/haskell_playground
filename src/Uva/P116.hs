{-# LANGUAGE TupleSections #-}
module Uva.P116 (spec) where

import Test.Hspec

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.List (minimumBy, intersperse)
import Data.Ord (comparing)

import Text.Parsec hiding (parse)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (integer)

type DPA = IOArray (Int, Int) (Int, Int)

mainS :: String -> IO String
mainS = return . format <=< mapM solve . parse

parse :: String -> [Array (Int, Int) Int]
parse = either undefined id . runParser parser () ""
  where
    parser :: Parsec String () [Array (Int, Int) Int]
    parser = do
      myb <- optionMaybe (count 2 number)
      case myb of
        Nothing -> return []
        Just nm -> do
          let n' = nm !! 0
              m' = nm !! 1
          ar <- listArray ((0, 0), (n' - 1, m' - 1)) `fmap` count (n' * m') number
          (ar:) `fmap` parser
    number :: Parsec String () Int
    number = fromInteger `fmap` integer haskell

format :: [([Int], Int)] -> String
format = join . map showCase
  where
    showCase (path, cost) = (join $ intersperse " " (map show path)) ++ "\n" ++
                            show cost ++ "\n"

---------------
-- algorithm --

solve :: Array (Int, Int) Int -> IO ([Int], Int)
solve arA = do
  let bnds@((_, _), (n, m)) = bounds arA
  arB <- newListArray bnds (repeat undefined) :: IO DPA

  -- run DP
  forM_ [0..n] $ \i ->
    writeArray arB (i, m) ((arA ! (i, m)), undefined)

  let neighbors i | i == 0 = [n, 0, 1]
                  | i == n = [n - 1, n, 0]
                  | otherwise = [i - 1, i, i + 1]

  forM_ (reverse [0..(m - 1)]) $ \j ->
    forM_ [0..n] $ \i -> do
      (mn, i') <- ($ neighbors i) $ (
        mapM $ \i' -> do
          (c, _) <- readArray arB (i', j + 1)
          return (c, i')
        ) >=> (
          return . minimumBy (comparing fst)
        )
      writeArray arB (i, j) (mn + (arA ! (i, j)), i')

  (mn, mn_i) <- ($ [0..n]) $ (
    mapM $ \i -> do
      (c, _) <- readArray arB (i, 0)
      return (c, i)
    ) >=> (
      return . minimumBy (comparing fst)
    )

  let backtrack :: (Int, Int) -> IO [Int]
      backtrack (i, j) | j == m = return [i]
                       | otherwise = do
        (_, next_i) <- readArray arB (i, j)
        (i:) `fmap` backtrack (next_i, j + 1)

  ((, mn) . map (+1)) `fmap` backtrack (mn_i, 0)


spec :: Spec
spec = do
  describe "mainS" $ do
    it "." $ do
      input <- readFile "./resources/UVA116.random.input"
      output <- mainS input
      expectation <- readFile "./resources/UVA116.random.alt.output"
      output `shouldBe` expectation
  describe "solve" $ do
    it "." $ do
      let ar = listArray ((0, 0), (4, 5)) [
            3, 4, 1, 2, 8, 6,
            6, 1, 8, 2, 7, 4,
            5, 9, 3, 9, 9, 5,
            8, 4, 1, 3, 2, 6,
            3, 7, 2, 8, 6, 4
            ]
      answer <- solve ar
      answer `shouldBe` ([1, 2, 3, 4, 4, 5], 16)
    it "." $ do
      let ar = listArray ((0, 0), (4, 5)) [
            3, 4, 1, 2, 8, 6,
            6, 1, 8, 2, 7, 4,
            5, 9, 3, 9, 9, 5,
            8, 4, 1, 3, 2, 6,
            3, 7, 2, 1, 6, 3
            ]
      answer <- solve ar
      answer `shouldBe` ([1, 2, 1, 5, 4, 5], 11)
    it "." $ do
      let ar = listArray ((0, 0), (1, 1)) [
            9, 10,
            9, 10
            ]
      answer <- solve ar
      answer `shouldBe` ([1, 2], 19)
