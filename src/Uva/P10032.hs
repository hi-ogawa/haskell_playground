{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Uva.P10032 (spec) where

import Test.Hspec

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Bits
import Data.List (minimumBy)
import Data.Ord (comparing)

type CompM a = IO a
type DPArray = IOArray (Int, Int) Integer

solve :: [Int] -> CompM (Int, Int)
solve people = do
  let n = length people
      m = 450 * 400
      weights = listArray (1, n) people
      wholeWeights = sum people

  -- construct array A satisfing:
  --   \forall l.
  --     A_{i, j} .&. (1 `shiftL` l) /= zeroBits <=>
  --     \exists X \subseteq {0..i} s.t. |X| == l and \sum_{x \in X}(W_x) == j
  (dpA :: DPArray) <- newListArray ((0, 0), (n, m - 1)) (repeat zeroBits)
  forM_ [0..n] $ \i -> writeArray dpA (i, 0) (bit 0)
  forM_ [1..n] $ \i ->
    forM_ [0..(m - 1)] $ \j -> do
      a <- readArray' dpA ((i - 1), (j - (weights ! i))) zeroBits
      a' <- readArray dpA ((i - 1), j)
      writeArray dpA (i, j) $ (a `shiftL` 1) .|. a'

  closestToHalf <- ($ [0..(m - 1)]) $ (
    filterM $ \j -> do
      a <- readArray dpA (n, j)
      return $ a .&. (((bit 0) `shiftL` (n `div` 2))) /= zeroBits
    ) >=> (
    return . minimumBy (comparing (abs . ((wholeWeights `div` 2) - )))
    )
  let two = [closestToHalf, wholeWeights - closestToHalf]
  return (minimum two, maximum two)
  where
    -- safe readArray
    readArray' :: (MArray a e m, Ix i) => a i e -> i -> e -> m e
    readArray' ar idx dflt = do
      bnds <- getBounds ar
      if inRange bnds idx
      then readArray ar idx
      else return dflt


----------
-- spec --

spec :: Spec
spec = do
  describe "solve" $ do
    it "." $ do
      answer <- solve [100, 90, 200]
      answer `shouldBe` (190, 200)
    it "." $ do
      answer <- solve [434, 287, 28, 116, 294, 386, 137, 193, 250, 272, 213, 428, 141, 260, 114, 77, 241, 127, 323, 137, 162, 369, 268, 430, 333, 81, 63, 374, 218, 236, 330, 253, 73, 409, 420, 418, 344, 107, 212, 143, 430, 424, 172, 120, 285, 338, 249, 75, 66, 121, 264, 227, 92, 81, 207, 424, 213, 321, 347, 32, 106, 226, 285, 178, 237, 306, 197, 130, 14, 408, 325, 46, 433, 46, 165, 268, 435, 15, 394, 51, 188, 259, 277, 279, 339, 85, 304, 102, 405, 200, 133, 61, 27, 19, 290, 263, 377, 37, 445, 390]
      answer `shouldBe` (11467, 11467)
