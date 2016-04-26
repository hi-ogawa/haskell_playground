module Uva.P10003 (spec) where

import Test.Hspec
import Control.Monad
import Data.Array
import Data.Array.IO
import Text.Parsec hiding (parse)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (natural)

type DPA = IOArray (Int, Int) Int

mainS :: String -> IO String
mainS = return . format <=< mapM (uncurry solve) . parse

parse ::  String -> [(Int, [Int])]
parse = either undefined id . runParser parser () ""
  where
    parser :: Parsec String () [(Int, [Int])]
    parser = do
      l <- number
      if l == 0
        then return []
        else do
        n <- number
        ps <- count n number
        ((l, ps):) `fmap` parser
    number :: Parsec String () Int
    number = fromInteger `fmap` natural haskell

format :: [Int] -> String
format = join . map showCase
  where
    showCase answer = "The minimum cutting is " ++ show answer ++ ".\n"

---------------
-- algorithm --

solve :: Int -> [Int] -> IO Int
solve l ps = do
  let n = length ps
      psA = listArray (0, n + 1) ([0] ++ ps ++ [l])
  a <- newListArray ((0, 0), (n + 1, n + 1)) (repeat undefined) :: IO DPA

  -- run DP --

  -- boundary
  forM_ [1..(n + 1)] $ \j -> writeArray a (j - 1, j) 0

  -- recursive relation
  forM_ [2..(n + 1)] $ \gap -> -- O(n)
    forM_ [0..(n + 1 - gap)] $ \i -> do -- O(n)
      let j = i + gap
      mn <- ($ [(i + 1)..(j - 1)]) $ ( -- 0(n)
        mapM $ \k -> liftM2 (+) (readArray a (i, k)) (readArray a (k, j))
        ) >=> (
        return . minimum
        )
      writeArray a (i, j) (mn + (psA ! j) - (psA ! i))

  -- read answer
  readArray a (0, n + 1)


----------
-- spec --

spec :: Spec
spec = do
  describe "mainS" $ do
    it "." $ do
      input <- readFile "./resources/UVA10003.critical.input"
      output <- mainS input
      expectation <- readFile "./resources/UVA10003.critical.output"
      output `shouldBe` expectation
    it "." $ do
      input <- readFile "./resources/UVA10003.random.input"
      output <- mainS input
      expectation <- readFile "./resources/UVA10003.random.output"
      output `shouldBe` expectation
  describe "solve" $ do
    it "." $ do
      answer <- solve 100 [25, 50, 75]
      answer `shouldBe` 200
    it "." $ do
      answer <- solve 10 [4, 5, 7, 8]
      answer `shouldBe` 22
