module VacationCalculator where

import Control.Monad
import Control.Monad.ST
import Data.Function (fix)
import Data.Maybe (listToMaybe)
import qualified Data.Vector.Mutable as V
import Test.Hspec


-- solve m [(m0, d0)..] = Right d
-- <=>
-- after working `m` month and taking vacation `d0` days at `m0`th month,
-- you can take `d` days vacation.
--
-- solve m [(m0, d0)..] = Left mx
-- <=>
-- after working `m` month and taking vacation `d0` days at `m0`th month,
-- you already broke rule at `mx`th month.

solve :: Int -> [(Int, Int)] -> Either Int Int
solve currentMonths takenVacations = runST solveST
  where
    vacations' = takeWhile (\(_, st, _) -> st <= currentMonths) vacations
    solveST :: ST s (Either Int Int)
    solveST = do
      a <- V.new (currentMonths + 1)
      V.write a 0 0
      forM_ [0..currentMonths] $ \m -> do
        let vToExpire = sum . map (\(v,_,_) -> v) $ filter (\(_, _, en) -> en == m) vacations'
            vToAdd    = sum . map (\(v,_,_) -> v) $ filter (\(_, st, _) -> st == m) vacations'
            vToUse    = sum . map (\(_,v) -> v) $ filter (\(m', _) -> m' == m) takenVacations
        v <- if m == 0 then return 0 else V.read a (m - 1)
        V.write a m ((v -.- vToExpire) + vToAdd - vToUse)
      m_m <- findM (\m -> (<$> V.read a m) (< 0)) [0..currentMonths]
      case m_m of
        Just m -> Left <$> return m
        Nothing -> Right <$> V.read a currentMonths
    findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
    findM f = (listToMaybe <$>) . filterM f
    (-.-) :: Int -> Int -> Int -- normalized subtraction
    x -.- y = max 0 (x - y)

vacations :: [(Int, Int, Int)]
vacations = map (\(vs, st, en) -> (vs, round (st * 12), round (en * 12))) $
  -- triple of
  --  - number of vacations
  --  - when it will be added
  --  - when it will expire
  [ (10, 0.5, 2.5)
  , (11, 1.5, 3.5)
  , (12, 2.5, 4.5)
  , (14, 3.5, 5.5)
  , (16, 4.5, 6.5)
  , (18, 5.5, 7.5) :: (Int, Float, Float)
  ] ++
  flip fix (20, 6.5, 8.5) (
    \l x@(vs, st, en) -> x : l (vs + 2, st + 1.0, en + 1.0)
  )


----------
-- spec --

spec :: Spec
spec =
  describe "solve" $ do
    it "." $
      solve 3 [] `shouldBe` Right 0
    it "." $
      solve 6 [] `shouldBe` Right 10
    it "." $
      solve 9 [(7, 8)] `shouldBe` Right 2
    it "." $
      solve 25 [(7, 8), (20, 12)] `shouldBe` Right 1
    it "." $
      solve 30 [(7, 8), (20, 11)] `shouldBe` Right 12
    it "." $
      solve 30 [(7, 8), (11, 3), (20, 11)] `shouldBe` Left 11
