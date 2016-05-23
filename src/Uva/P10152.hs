{-# LANGUAGE TupleSections #-}
module Uva.P10152 where

-- greedy

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.ST
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (Down(..))
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V
import GHC.Exts (sortWith)
import Test.Hspec


type A s = V.STVector s Int

solve :: [String] -> [String] -> [String]
solve xs ys = runST $ solveST xs ys

solveST :: [String] -> [String] -> ST s [String]
solveST inits finals = do
  let n = length inits
  a <- V.new n
  forM_ (zip inits [0..(n-1)]) $ \(t, i) ->
    V.write a i . fromJust $ elemIndex t finals
  map (finals !!) <$> solveST' a


-- solve integer representation
solveST' :: A s -> ST s [Int]
solveST' a = do
  let n = V.length a
  result <- newSTRef []
  forM_ (reverse [0..(n-1)]) $ \e -> do
    i <- v_elemIndex a e
    when ([(i+1)..e] /= []) $ do
      (mn, mx) <- ($ [(i+1)..e]) $
        mapM (V.read a) >=>
        return . (minimum &&& maximum)
      ($ [0..e]) $
        filterM (\i' -> do
          x <- V.read a i'
          return $ mn <= x && x <= mx
        ) >=>
        mapM (\i' -> (i',) <$> V.read a i') >=>
        return . sortWith (Down . snd) >=>
        mapM_ (\(i', e') -> do
          move a i'
          modifySTRef result (e':)
        )
  reverse <$> readSTRef result
  where
    move :: A s -> Int -> ST s ()
    move _a i = forM_ (reverse [1..i]) $ \i' -> V.swap _a i' (i' - 1)
    v_elemIndex :: A s -> Int -> ST s Int
    v_elemIndex _a e = head <$> filterM (((e ==) <$>) . V.read _a) [0..((V.length _a)-1)]


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

          answer = [ "Sir Lancelot    "
                   , "Richard M. Nixon"
                   , "Yertle          " ]
      solve xs ys `shouldBe` answer
