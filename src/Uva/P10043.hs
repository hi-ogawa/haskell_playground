{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Uva.P10043 (main, spec) where

-- spec
import Test.Hspec

-- debug
import Debug.Trace (trace)

-- non-exhaustive pattern match
import Control.Exception (assert)

import Control.Arrow ((>>>), (&&&))
import Control.Monad (join, zipWithM_, when, void)
import Control.Monad.State (State, evalState)
import Control.Lens (makeLenses, (.=), (%=), use, to)
import Data.Function ((&))

import Data.Array.Diff

-- TODO: check the difference of input consumption by B.getContents
-- import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Monad.Writer (Writer, execWriter, tell, lift)
import Text.Parsec hiding (State)

type Idx = (Int, Int)
type BoolArray = DiffUArray Idx Bool
type IntArray = DiffUArray Idx Int

-- only used in smartCalcVertical'
data St = St {
  _mx :: Int,
  _stack :: [(Int, Int)]
}
makeLenses ''St


-----------
-- debug --

debug :: Show a => a -> a
debug x = trace (show x) x

debugM :: Show a => Monad m => a -> m a
debugM x = trace (show x) (return x)


----------------
-- formatting --

main :: IO ()
main = B.getContents >>= (mainPure >>> B.putStr)

mainPure :: B.ByteString -> B.ByteString
mainPure = execWriter . runParserT pMain () ""

pMain :: Stream s (Writer B.ByteString) Char => ParsecT s () (Writer B.ByteString) ()
pMain = do
  nCases <- pNum; newline
  range (1, nCases) & mapM_ (\iCase -> do
    lift . tell . (`B.append` "\n") =<< pCase
    when (iCase /= nCases) $ void newline
    )

pCase :: Monad m => Stream s m Char => ParsecT s () m B.ByteString
pCase = pArray >>= return . B.pack . show . solve

pArray :: Monad m => Stream s m Char => ParsecT s () m BoolArray
pArray = do
  l <- pNum; space; w <- pNum; newline
  -- trees around boundary doesn't affect area. that's why it's not ((0, 0), (l, w))
  -- TODO: handle the case where l == 1 or w == 1
  pTrees $ listArray ((1, 1), (l - 1, w - 1)) (repeat True)

-- tail recursion
pTrees :: Monad m => Stream s m Char => BoolArray -> ParsecT s () m BoolArray
pTrees bAr = do
  n <- pNum
  case n of
    0 -> return bAr
    1 -> do
      space; i <- pNum; space; j <- pNum; newline
      pTrees $ bAr //? [((i, j), False)]
    k -> do
      space; i <- pNum; space; j <- pNum; space; di <- pNum; space; dj <- pNum; newline
      pTrees $ bAr //? (range (0, k - 1) & map (\t -> ((i + t * di, j + t * dj), False)))
  where
    (//?) :: BoolArray -> [(Idx, Bool)] -> BoolArray
    (//?) ar = (ar //) . filter (\(ij, _) -> inRange (bounds ar) ij)

pNum :: Monad m => Stream s m Char => ParsecT s () m Int
pNum = do
  f <- (char '-' >> return negate) <|> (return id)
  return . f . read =<< many digit


----------------
-- algorithms --

solve :: BoolArray -> Int
solve = smartSolve

---------------
-- smart one --
-- (learnt from http://www.drdobbs.com/database/the-maximal-rectangle-problem/184410529 )

smartSolve :: BoolArray -> Int
smartSolve bAr =
  (range (jMin, jMax) &) $   -- O(m)
    -- map (dumbCalcVertical iAr) >>>
    map (smartCalcVertical iAr) >>>
    maximum
  where
    iAr = preprocess bAr
    ((_, jMin), (_, jMax)) = bounds bAr


-- O(n * m)
-- TODO: this `m` times array update might be bottleneck
preprocess :: BoolArray -> IntArray
preprocess bAr =
  (range (jMin, jMax) &) $
    flip foldl zeroAr $ \iAr j ->
      iAr // (map (id &&& calcNext iAr) (range ((iMin, j), (iMax, j))))
  where
    rng@((iMin, jMin), (iMax, jMax)) = bounds bAr
    zeroAr = amap (const 0) bAr
    calcNext :: IntArray -> Idx -> Int
    calcNext iAr (i, j)
      | (bAr ! (i, j)) && inRange rng (i, j - 1) = 1 + iAr ! (i, j - 1)
      | (bAr ! (i, j))                           = 1
      | otherwise                                = 0


-- O(n^2)
dumbCalcVertical :: IntArray -> Int -> Int
dumbCalcVertical iAr j =
  (range (iMin, iMax) &) $  -- O(n)
    map (\i ->
      let w = iAr ! (i, j)
          h = (range' (i, iMax) &) $  -- O(n)
                takeWhile (\i' -> w <= iAr ! (i', j)) >>>
                length
          h' = (range' (i, iMin) &) $ -- O(n)
                 takeWhile (\i' -> w <= iAr ! (i', j)) >>>
                 length
      in (w + 1) * ((h + h' - 1) + 1)
    ) >>>
    maximum
  where
    ((iMin, _), (iMax, _)) = bounds iAr
    range' (i_, j_) | i_ <= j_  = takeWhile (<= j_) $ iterate (+1) (i_)
                    | otherwise = takeWhile (>= j_) $ iterate (\x -> x - 1) (i_)

-- O(n)
-- TODO: implement smarter version of `smartCalcVertical`
smarterCalcVertical :: IntArray -> Int -> Int
smarterCalcVertical iAr j = undefined

-- O(n)
-- TODO: it makes unnecessary copy of list from one column of array, which might be bottleneck
smartCalcVertical :: IntArray -> Int -> Int
smartCalcVertical iAr j =
  smartCalcVertical' . map (iAr !) $ range ((iMin, j), (iMax, j))
  where
    ((iMin, _), (iMax, _)) = bounds iAr

smartCalcVertical' :: [Int] -> Int
smartCalcVertical' ls = evalState calc St {_mx = undefined, _stack = undefined}
  where
    calc = do
      mx .= 0
      stack .= [(0, 0)]
      zipWithM_ step (ls ++ [0]) (iterate (+1) 1)
      use mx
    step :: Int -> Int -> State St ()
    step w i = do
      (w', _) <- use $ stack.to head
      case compare w' w of
        LT -> stack %= ((w, i):)
        EQ -> stack %= ((w, i):)
        GT -> do
          (popped, stk'@((_, i'):_)) <- use $ stack.to (span (\(w'', _) -> w < w''))
          mx %= max (step' i' i popped)
          stack .= ((w, i):stk')
    step' :: Int -> Int -> [(Int, Int)] -> Int
    step' iBottom iTop ((w, _):tl) = step'' iBottom iTop tl w
    step' _ _ _ = assert False undefined
    step'' :: Int -> Int -> [(Int, Int)] -> Int -> Int
    step'' iBottom iTop [] currentW = size iBottom iTop currentW
    step'' iBottom iTop ((w, i):tl) currentW
      | w < currentW = max (size i iTop currentW) (step'' iBottom iTop tl w)
      | otherwise    = step'' iBottom iTop tl currentW
    size i0 i1 w = (i1 - i0) * (w + 1)


--------------
-- dumb one --

-- O(n^3 * m^3)
dumbSolve :: BoolArray -> Int
dumbSolve ar =
  (range ((iMin, jMin), (iMax, jMax)) &) $      -- O(n * m)
    map (\ij ->
      (range (ij, (iMax, jMax)) &) $            -- O(n * m)
        filter (all (ar !) . range . (ij,)) >>> -- O(n * m)
        map (size ij)
    ) >>>
    join >>>
    maximum . (atLeast:)
  where
    ((iMin, jMin), (iMax, jMax)) = bounds ar
    atLeast = 2 + max (iMax - iMin) (jMax - jMin)
    size (i, j) (i', j') = ((i' - i) + 2) * ((j' - j) + 2)


----------
-- spec --

-- TODO: check memory usage/time performance of
--   Array
--   DiffArray
--   DiffUArray
--   STArray
--   STUArray
spec :: Spec
spec = do
  describe "dumbSolve" $ it "." $ dumbSolve ar0 `shouldBe` 9
  describe "smartSolve" $ do
    describe "preprocess"        $ it "." $ (show $ preprocess ar0) `shouldBe` (show ar1)
    describe "dumbCalcVertical"  $ it "." $ dumbCalcVertical ar1 0 `shouldBe` 4
    describe "smartCalcVertical" $ it "." $ smartCalcVertical ar1 0 `shouldBe` 4
    describe "smartCalcVertical'" $ do
      it "." $ smartCalcVertical' [1, 2, 2, 4, 3] `shouldBe` (4 + 1) * (2 + 1)
      it "." $ smartCalcVertical' [1, 2, 2, 10, 3] `shouldBe` (1 + 1) * (10 + 1)
      it "." $ smartCalcVertical' [5, 5, 5, 4, 4, 5] `shouldBe` (6 + 1) * (4 + 1)
    -- it "." $ -- 10 sec in ghci
    --   smartSolve (listArray ((1, 1), (1000, 1000)) (repeat True)) `shouldBe` (1001 * 1001)
    it "." $ -- 1 sec in ghci
      let i = listArray ((1, 1), (13, 9163)) (repeat True) //
                (map (id &&& (const False)) [(9, 852), (11, 166), (3, 9144), (9, 8938)])
      in smartSolve i `shouldBe` 113204
  describe "mainPure" $ do
    it "." $ mainPure i0 `shouldBe` o0
    -- it "." $ mainPure i1 `shouldBe` o1 -- 48 sec in ghci
    -- it "." $ do -- 48 * 10 or something
    --   i <- B.readFile "./resources/UVA10043.input"
    --   o <- B.readFile "./resources/UVA10043.output"
    --   mainPure i `shouldBe` o
  where
    ar0 :: BoolArray
    ar0 = listArray ((0, 0), (2, 3)) $ map (=="o") . words $ str
      where
        str = "o x x o" ++ "\n" ++
              "x o o o" ++ "\n" ++
              "x o o x"
    ar1 :: IntArray
    ar1 = listArray ((0, 0), (2, 3)) $ map read . words $ str
      where
        str = "1 0 0 1" ++ "\n" ++
              "0 1 2 3" ++ "\n" ++
              "0 1 2 0"
    i0 = B.fromChunks [
        "2"        , "\n",
        "2 3"      , "\n",
        "0"        , "\n",
        "10 10"    , "\n",
        "2 1 1 8 0", "\n",
        "2 1 9 8 0", "\n",
        "0"
      ]
    o0 = B.fromChunks [
        "6"       , "\n",
        "80"      , "\n"
      ]
    i1 = B.fromChunks [
        "1"                  , "\n",
        "2467 1797"          , "\n",
        "1 1655 600"         , "\n",
        "2 1369 902 -371 -54", "\n",
        "0"
      ]
    o1 = B.fromChunks [
        "2207965", "\n"
      ]
