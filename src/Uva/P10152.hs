{-# LANGUAGE TupleSections #-}
module Uva.P10152 (spec) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (Down(..))
import GHC.Exts (sortWith)
import Test.Hspec

solve :: [String] -> [String] -> [String]
solve xs ys =
  let xs' = map (\x -> fromJust $ elemIndex x ys) xs
  in
  map (ys !!) . sortWith Down $ f xs'
  where
    f :: [Int] -> [Int]
    f ls = g (reverse ls) (length ls - 1)
    g :: [Int] -> Int -> [Int]
    g []     _             = []
    g (h:ls) m | h == m    = g ls (m - 1)
               | otherwise = h : g ls m

----------
-- spec --

spec :: Spec
spec =
  describe "solve" $ do

    it "." $ do
      let xs = [ "Yertle"
               , "Duke of Earl"
               , "Sir Lancelot" ]

          ys = [ "Duke of Earl"
               , "Yertle"
               , "Sir Lancelot" ]

          answer = ["Duke of Earl"]
      solve xs ys `shouldBe` answer

    it "." $ do
      let xs = [ "Yertle           "
               , "Duke of Earl     "
               , "Sir Lancelot     "
               , "Elizabeth Windsor"
               , "Michael Eisner   "
               , "Richard M. Nixon "
               , "Mr. Rogers       "
               , "Ford Perfect     "
               , "Mack             " ]

          ys = [ "Yertle           "
               , "Richard M. Nixon "
               , "Sir Lancelot     "
               , "Duke of Earl     "
               , "Elizabeth Windsor"
               , "Michael Eisner   "
               , "Mr. Rogers       "
               , "Ford Perfect     "
               , "Mack             " ]

          answer = [ "Sir Lancelot     "
                   , "Richard M. Nixon "
                   , "Yertle           " ]
      solve xs ys `shouldBe` answer
