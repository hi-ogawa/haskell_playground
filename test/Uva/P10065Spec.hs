module Uva.P10065Spec where

import Uva.P10065

import Control.Monad.Identity
import Text.Parsec (runParserT)

import Test.Hspec

spec :: Spec
spec = do
  describe "hull" $ it "." ex0
  describe "solve" $ do
    describe "case 0" $ it "." ex1
    describe "case 1" $ it "." ex2
  describe "pCase" $ it "." ex3
  describe "mainPure" $ it "." ex4

ex0 :: Expectation
ex0 = hull i `shouldBe` o
  where
    i = [(2, 1), (2, 2), (1, 3), (3, 2), (3, 2.5), (2, 5), (2.5, 3), (4, 5)]
    o = [(1, 3), (2, 1), (3, 2), (4, 5), (2, 5)]

ex1 :: Expectation
ex1 = solve i `shouldBe` o
  where
    i = [(2, 2), (2, 0), (0, 0), (0, 2), (1, 3)]
    o = 0.0

ex2 :: Expectation
ex2 = solve i `shouldBe` o
  where
    i = [(0, 0), (0, 3), (1, 4), (2, 3), (4, 4), (4, 1), (2, 1)]
    o = 18.518518518518523

ex3 :: Expectation
ex3 =
  runParserT pCase () "" i `shouldBe` Identity (Right o)
  where
    i =
      "7"   ++ "\n" ++
      "0 0" ++ "\n" ++
      "0 3" ++ "\n" ++
      "1 4" ++ "\n" ++
      "2 3" ++ "\n" ++
      "4 4" ++ "\n" ++
      "4 1" ++ "\n" ++
      "2 1" ++ "\n"
    o = 18.518518518518523

ex4 :: Expectation
ex4 = do
  input <- readFile "./resources/UVA10065.input"
  output <- readFile "./resources/UVA10065.output"
  mainPure input `shouldBe` output
