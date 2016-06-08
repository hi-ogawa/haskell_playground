module Whatever where

import Control.Arrow ((>>>))
import Test.Hspec

decimal2hex :: Int -> String
decimal2hex num =
  ($ [(0::Int)..]) $
    map (16^) >>>
    takeWhile ((0 /=). (num `div`)) >>>
    reverse >>>
    g num >>>
    map f
  where
    f :: Int -> Char
    f 10 = 'A'
    f 11 = 'B'
    f 12 = 'C'
    f 13 = 'D'
    f 14 = 'E'
    f 15 = 'F'
    f i  = head (show i)
    g :: Int -> [Int] -> [Int]
    g _ [] = []
    g x (h:l) = let (d, m) = x `divMod` h in
                d : g m l

spec :: Spec
spec = do
  describe "decimal2hex" $ do
    it "." $ decimal2hex 284 `shouldBe` "11C"
