module Uva.P10131 (spec) where

import Test.Hspec
import Control.Monad
import Data.Function (fix)
import Data.IORef
import qualified Data.Array.IO as A
import qualified Data.Vector.Mutable as V

solve :: [(Int, Int)] -> IO [Int]
solve elephants = do
  let n = length elephants -- 0..1000

  -------------------------------
  -- 1. construct graph O(n^2) --
  graph <- V.replicate n []
  graphMtrx <- A.newListArray ((0, 0), (n - 1, n - 1)) (repeat False) :: IO (A.IOArray (Int, Int) Bool)
  forM_ (zip elephants [0..]) $ \((w, s), i) ->
    forM_ (zip elephants [0..]) $ \((w', s'), j) ->
      when (w < w' && s > s') $ do
        V.modify graph (j:) i
        A.writeArray graphMtrx (i, j) True


  ----------------------------------
  -- 2. topological sort O(n + m) --
  discovered <- V.replicate n False
  orderedVs <- V.replicate n 0
  orderIdx <- newIORef (n - 1)

  -- dfs
  ($ [0..(n - 1)]) $ (
    mapM_ $ fix $ \dfs i -> -- super inline recursion
      (V.read discovered i >>=) $ flip unless $ do
        V.write discovered i True
        (`forM_` dfs) =<< V.read graph i
        (flip (V.write orderedVs) i) =<< readIORef orderIdx
        modifyIORef orderIdx (\x -> x - 1)
    )

  -------------------------------------
  -- 3. longest path (dp) O(n^2) --
  dpA <- V.replicate n 0 :: IO (V.IOVector Int)
  dpAPtr <- V.replicate n Nothing :: IO (V.IOVector (Maybe Int))

  forM_ [0..(n - 1)] $ \i -> do
    forM_ [0..(i - 1)] $ \j -> do
      vi <- V.read orderedVs i
      vj <- V.read orderedVs j
      (A.readArray graphMtrx (vj, vi) >>=) $ flip when $ do
        aj <- V.read dpA j
        V.modify dpA (max (aj + 1)) i
        V.write dpAPtr i (Just j)

  -- terminal of longest path
  trmnl <- ($ [1..(n - 1)]) $ flip foldM 0 $ \i j -> do
    ai <- V.read dpA j
    aj <- V.read dpA j
    if ai > aj then return i else return j

  -- construct longest path by backtracking from terminal
  (($ []) . ($ trmnl)) $ fix $ \bktk i path -> do
    v <- (+1) `fmap` V.read orderedVs i
    (V.read dpAPtr i >>=) $ maybe (return (v:path)) $ \j ->
      bktk j (v:path)


spec :: Spec
spec = do
  describe "solve" $ do
    it "." $ do
      answer <- solve [(6008, 1300), (6000, 2100), (500 , 2000), (1000, 4000), (1100, 3000), (6000, 2000), (8000, 1400), (6000, 1200), (2000, 1900)]
      answer `shouldBe` [4,5,2,1]
    it "." $ do
      answer <- solve [(4,6),(5,5),(1,9),(3,7),(2,8),(0,10)]
      answer `shouldBe` [6, 3, 5, 4, 1, 2]
