module Uva.P10084 (spec) where

import Data.List (inits, sortBy)
import Data.Maybe (mapMaybe)
import GHC.Exts (sortWith)
import Test.Hspec

type Point = (Float, Float)
type Vector = (Float, Float)
type HalfPlane = (Vector, Point)
data Call = Hotter | Colder

solve :: [(Point, Call)] -> [Float]
solve calls =
  let hps = map (\(p, (p', c)) -> calculateHalfPlain p p' c) $
              zip ((0,0):map fst calls) calls
  in
  map (polygonArea . halfPlanes2Polygon) $ tail (inits hps)

calculateHalfPlain :: Point -> Point -> Call -> HalfPlane
calculateHalfPlain (x, y) (x', y') call =
  let a = x' - x
      b = y' - y
      c = (x + x') / 2
      d = (y + y') / 2 in
  case call of
    Hotter -> (( a,  b), (c, d))
    Colder -> ((-a, -b), (c, d))

-- O(n^3)
halfPlanes2Polygon :: [HalfPlane] -> [Point]
halfPlanes2Polygon hps =
  let hps' = defaultHalfPlanes ++ hps
      pts = crossings hps'
      pts' = filter (\pt -> all (include pt) hps') pts in
  pts'
  where
    defaultHalfPlanes :: [HalfPlane]
    defaultHalfPlanes =
      [ ((1, 0), (0, 0))
      , ((0, 1), (0, 0))
      , ((-1, 0), (10, 10))
      , ((0, -1), (10, 10))
      ]

-- TODO: is it crucial to deal with "more than 2 lines cross at the same point" ?
-- TODO: find algorithm for better time complexity
crossings :: [HalfPlane] -> [Point]
crossings hps = mapMaybe (uncurry crossing) $ pairs hps

-- list comprehension specification: https://www.haskell.org/onlinereport/exps.html#sect3.11
-- un-ordered pair
pairs :: [a] -> [(a, a)]
pairs ls =
  let n = length ls in
  [ (ls !! i, ls !! j) | i <- [0..(n-1)], j <- [(i+1)..(n-1)]]

crossing :: HalfPlane -> HalfPlane -> Maybe Point
crossing ((a, b), (c, d)) ((a', b'), (c', d')) =
  let k = - (a * c + b * d)
      k' = - (a' * c' + b' * d')
      divisor = a * b' - a' * b in
  if divisor == 0
  then Nothing
  else Just ((b * k' - b' * k) / divisor, (a' * k - a * k') / divisor)

-- TODO: what exactly `>= :: Float -> Float -> Float` does?
include :: Point -> HalfPlane -> Bool
include (x, y) ((a, b), (c, d)) = a * x + b * y - (a * c + b * d) >= 0

-- `pts` forms already convex polygon, which might not be sorted counter-clockwise
polygonArea :: [Point] -> Float
polygonArea pts
  | length pts <= 2 = 0
  | otherwise = signedArea . sortPoints $ pts

sortPoints :: [Point] -> [Point]
sortPoints pts =
  let leftMostPt:otherPts = sortWith fst pts in
  leftMostPt:sortBy (compare2d leftMostPt) otherPts

compare2d :: Point -> Point -> Point -> Ordering
compare2d p0 p1 p2 = compare 0 (signedArea [p0, p1, p2])

signedArea :: [Point] -> Float
signedArea pts =
  sum . map (uncurry area) $ zip pts (tail pts ++ [head pts])
  where
    area :: Point -> Point -> Float
    area (x, y) (x', y') = (x * y' - x' * y) / 2


----------
-- spec --

spec :: Spec
spec = do
  describe "pairs'" $ it "." $
    pairs [0..3::Int] `shouldBe` [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]
  describe "crossing" $ it "." $
    crossing ((1, -1), (1, 0.5)) ((1, 1), (-1, 0)) `shouldBe` Just (-0.25, -0.75)
  describe "include" $ it "." $
    include (2, 0) ((1, -1), (1, 0.5)) `shouldBe` True
  describe "solve" $ it "." $
    let input = [ ((10.0, 10.0), Colder)
                , ((10.0, 0.0 ), Hotter)
                , ((0.0 , 0.0 ), Colder)
                , ((10.0, 10.0), Hotter)
                ] in
    solve input `shouldBe` [50.0,37.5,12.5,0.0]
  describe "halfPlanes2Polygon" $ it "." $
    halfPlanes2Polygon [] `shouldBe` [(0.0,0.0),(-0.0,10.0),(10.0,0.0),(10.0,10.0)]
