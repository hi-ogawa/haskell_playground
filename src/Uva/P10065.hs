module Uva.P10065 (main, spec) where

-- Lesson:
--   - haskell tips
--     - default lazily consumed input (with demo)
--     - mock output by writer monad in parsec
--     - unit test on hspec (with cabal cli)
--     - print floating value
--     - readFile relatively
--     - assertion runtime error (to ignore warning)
--     - do syntax indentation
--   - algorithm:
--     - triangle signed area calculation for sorting points counter clockwise

import Control.Exception (assert)

import Test.Hspec
import Control.Monad.Identity

import Control.Monad.Writer (Writer, execWriter, tell, lift)
import Text.Parsec
import Numeric (showFFloat)

import Data.List (sortBy, minimumBy, delete)

type Point = (Double, Double)

----------------
-- formatting --
----------------

type MockO = Writer String

putStrMock :: String -> MockO ()
putStrMock = tell

-- NOTE: lazily input is consumed and calculation is done
main :: IO ()
main = putStr . mainPure =<< getContents

mainPure :: String -> String
mainPure = execWriter . runParserT pMain () ""

pMain :: ParsecT String () MockO ()
pMain = pMainRec 1

pMainRec :: Int -> ParsecT String () MockO ()
pMainRec i =
  do nPts <- lookAhead pNum
     if nPts == (0 :: Int)
     then do
       anyChar; eof
     else do
       result <- pCase
       lift . putStrMock $ format i result
       pMainRec (i + 1)
  where
    format idx d =
      "Tile #" ++ show idx ++ "\n" ++
      "Wasted Space = " ++ (showFFloat (Just 2) d $ "") ++ " %\n\n"


pCase :: Monad m => ParsecT String () m Double
pCase =
  do nPts <- pNum; newline
     pts <- count nPts $ do
       x <- pNum; space; y <- pNum; newline
       return (x, y)
     return $ solve pts


pNum :: Monad m => Read a => Num a => ParsecT String () m a
pNum = read `fmap` many digit


---------------
-- algorithm --
---------------

solve :: [Point] -> Double
solve pts = 100 * (1 - ((abs $ signedArea pts) / (signedArea $ hull pts)))

-- 2d Graham scan (https://en.wikipedia.org/wiki/Graham_scan)
-- input: 2d points
-- output: verteces forming convex polygon ordered couter clockwise (starting from left most)
-- assume
--  - `length pts >= 3`
--  - no three points on the same line
hull :: [Point] -> [Point]
hull pts = reverse result
  where
    origin = minimumBy (\p p' -> compare (fst p) (fst p')) pts
    sortedPts = sortBy (compare2d origin) (delete origin pts)
    (first2, rest) = splitAt 2 sortedPts
    result = foldl (\stack p -> convexize (p:stack)) (reverse (origin:first2)) rest

convexize :: [Point] -> [Point]
convexize stack@(p2:p1:p0:ls)
  | signedArea [p0, p1, p2] > 0 = stack
  | otherwise                   = convexize (p2:p0:ls)
convexize _ = assert False undefined

---------------
-- utilities --
---------------

-- assume three points are not degenerete
compare2d :: Point -> Point -> Point -> Ordering
compare2d p0 p1 p2 = compare 0 (signedArea [p0, p1, p2])

-- assume `length pts >= 3`
signedArea :: [Point] -> Double
signedArea pts =
  sum . map (uncurry area) $ zip pts (tail pts ++ [head pts])
  where
    area :: Point -> Point -> Double
    area (x, y) (x', y') = x * y' - x' * y


----------
-- spec --

spec :: Spec
spec = do
  describe "hull" $ it "." ex0
  describe "solve" $ do
    it "." ex1
    it "." ex2
  describe "pCase" $ it "." ex3
  describe "mainPure" $ it "." ex4
  where
    ex0 = hull i `shouldBe` o
      where
        i = [(2, 1), (2, 2), (1, 3), (3, 2), (3, 2.5), (2, 5), (2.5, 3), (4, 5)]
        o = [(1, 3), (2, 1), (3, 2), (4, 5), (2, 5)]

    ex1 = solve i `shouldBe` o
      where
        i = [(2, 2), (2, 0), (0, 0), (0, 2), (1, 3)]
        o = 0.0

    ex2 = solve i `shouldBe` o
      where
        i = [(0, 0), (0, 3), (1, 4), (2, 3), (4, 4), (4, 1), (2, 1)]
        o = 18.518518518518523

    ex3 =
      runParserT pCase () "" i `shouldBe` Identity (Right o)
      where
        i =
          "7"   ++ "\n" ++
          "0 0" ++ "\n" ++
          "0 3" ++ "\n" ++
          "1 4" ++ "\n" ++
          "2 3" ++ "\n" ++
          "4 4" ++ "\n" ++
          "4 1" ++ "\n" ++
          "2 1" ++ "\n"
        o = 18.518518518518523

    ex4 = do
      input <- readFile "./resources/UVA10065.input"
      output <- readFile "./resources/UVA10065.output"
      mainPure input `shouldBe` output
