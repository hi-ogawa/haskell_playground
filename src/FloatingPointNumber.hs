module FloatingPointNumber (spec) where

import Test.Hspec

-- Reference:
-- http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
-- http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:RealFloat

f :: Float
f = 0.01

spec :: Spec
spec = do
  describe "type Float" $ do
    describe "floatRadix" $ it "is 2" $
      floatRadix f `shouldBe` 2
    describe "floatDigits (the number of digits for significand)" $ it "is 24" $
      floatDigits f `shouldBe` 24
    describe "Examples" $ do
      let l = 1 :: Float
      it "1 /= 1 + 1 / 2 ^ 23" $ l /= 1 + 1 / 2 ^ 23
      it "1 == 1 + 1 / 2 ^ 24" $ l == 1 + 1 / 2 ^ 24
      it "1 /= 1.0000001"      $ l /= 1.0000001
      it "1 == 1.00000001"     $ l == 1.00000001
      it "(1 / 2 ^ 23) ^ 7 /= (1 / 2 ^ 23) ^ 6" $ (1 / 2 ^ 23) ^ 7 /= ((1 / 2 ^ 23) ^ 6 :: Float)
      it "(1 / 2 ^ 23) ^ 7 == (1 / 2 ^ 23) ^ 8" $ (1 / 2 ^ 23) ^ 7 == ((1 / 2 ^ 23) ^ 8 :: Float)
      it "2 ^ (2 ^ 7) ~ 1e39 isInfinite" $
        isInfinite (2 ^ (2 ^ 7) :: Float) &&
        isInfinite (1e39 :: Float)
      it "2 ^ (2 ^ 6) is not Infinite" $
        not $ isInfinite (2 ^ (2 ^ 6) :: Float)
