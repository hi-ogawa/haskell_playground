module Uva.P10041 (spec) where

import Test.Hspec
import Control.Monad (join)
import qualified Data.Heap as H

mainPure :: String -> String
mainPure input =
  let answers = map (show . solve . map read . tail . words) . tail . lines $ input in
  join $ map (++ "\n") answers

-- use heapsort since finding smaller half elements is enough
solve :: [Int] -> Int
solve ss =
  let r = length ss
      hp = H.fromList ss
      mid = H.minimum . H.drop (r `div` 2) $ hp in
  sum . map (abs . (mid -)) $ ss

spec :: Spec
spec = do
  describe "mainPure" $ do
    it "." $ do
      input <- readFile "./resources/UVA10041.input"
      output <- readFile "./resources/UVA10041.output"
      mainPure input `shouldBe` output
    it "." $ do
      input <- readFile "./resources/UVA10041.random.input"
      output <- readFile "./resources/UVA10041.random.output"
      mainPure input `shouldBe` output
  describe "solve" $ do
    it "." $ solve [2, 4] `shouldBe` 2
    it "." $ solve [2, 4, 6] `shouldBe` 4
