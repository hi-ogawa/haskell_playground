module Uva.P10054Spec where

import Uva.P10054

import Test.Hspec

spec :: Spec
spec = do
  describe "mainPure" $ it "." ex

ex :: Expectation
ex = do
  input <- readFile "./resources/UVA10054.input"
  output <- readFile "./resources/UVA10054.output"
  mainPure input `shouldBe` output
