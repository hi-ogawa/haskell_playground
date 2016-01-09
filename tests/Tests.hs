module Main where

-- import qualified UVA10054 as Q
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "descipriont of hspec" $ do
    it "" $ do
      (0 :: Int) `shouldBe` (0 :: Int)
