module BentleyOttmann (spec) where

import Prelude hiding (lines)
import Data.List (delete, elemIndex)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import Data.Function (fix)

import Test.Hspec

type X = Double
type Y = Double
type Point = (X, Y)
type Line = (Point, Point) -- two endpoints from left to right (assume non-vertical)

-- left endpoint, right endpoint, intersection
data XElem = LeftEnd Point Line
           | RightEnd Point Line
           | Intersect Point Line Line -- Intersect p l0 l1 represents l0 is above l1 at the left of p
             deriving (Show, Eq)

xelem2x :: XElem -> X
xelem2x (LeftEnd (x, _) _) = x
xelem2x (RightEnd (x, _) _) = x
xelem2x (Intersect (x, _) _ _) = x


---------------------------------------------
-- interfaces for X-structure, Y-structure --

-- TODO: use ideal data structure

type XColl = [XElem] -- ideally heap
type YColl = [Line]  -- ideally binary tree

xInsert :: XElem -> XColl -> XColl
xInsert h tl = sortWith xelem2x $ (h:tl)

xDelete :: XElem -> XColl -> XColl
xDelete = delete

xDropMin :: XColl -> Maybe (XElem, XColl)
xDropMin [] = Nothing
xDropMin (h:tl) = Just (h, tl)

xInitialize :: [Line] -> XColl
xInitialize = sortWith xelem2x . map (\l@(p, _) -> LeftEnd p l)

-- detect neighbor changes along with each YColl operation
yInsert :: X -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
yInsert x l ycoll =
  let
    ycoll' = sortWith (snd . fromJust . intersectX x) (l:ycoll)
    Just i = elemIndex l ycoll'
    neighbors0 = if i /= 0 then [(ycoll' !! (i - 1), l)]
                 else []
    neighbors1 = if i /= length ycoll' - 1 then [(l, ycoll' !! (i + 1))]
                 else []
    nonNeighbors = if i /= 0 && i /= length ycoll' - 1 then [(ycoll' !! (i - 1), ycoll' !! (i + 1))]
                   else []
  in
  (ycoll', neighbors0 ++ neighbors1, nonNeighbors)

-- return `nonNeighbors` for consistent interface even though it's not necessary for Bentley-Ottmann algorithm
yDelete :: X -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
yDelete _ l ycoll =
  let
    Just i = elemIndex l ycoll
    neighbors = if i /= 0 && i /= length ycoll - 1 then [(ycoll !! (i - 1), ycoll !! (i + 1))]
                else []
    nonNeighbors0 = if i /= 0 then [(ycoll !! (i - 1), l)]
                    else []
    nonNeighbors1 = if i /= length ycoll - 1 then [(l, ycoll !! (i + 1))]
                    else []
    ycoll' = delete l ycoll
  in
  (ycoll', neighbors, nonNeighbors0 ++ nonNeighbors1)

-- assume l0 is above l1 at the left of x
ySwap :: X -> Line -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
ySwap _ l0 l1 ycoll =
  let
    Just i0 = elemIndex l0 ycoll
    Just i1 = elemIndex l1 ycoll
    (neighbors0, nonNeighbors0) = if i0 /= 0
                                  then ([(ycoll !! (i0 - 1), l1)], [(ycoll !! (i0 - 1), l0)])
                                  else ([], [])
    (neighbors1, nonNeighbors1) = if i1 /= length ycoll - 1
                                  then ([(l0, (ycoll !! (i1 + 1)))], [(l1, (ycoll !! (i1 + 1)))])
                                  else ([], [])
    ycoll' = map (ycoll !!) $ [0..(i0 - 1)] ++ [i1, i0] ++ [(i1 + 1)..(length ycoll - 1)]
  in
  (ycoll', [(l1, l0)] ++ neighbors0 ++ neighbors1
         , [(l0, l1)] ++ nonNeighbors0 ++ nonNeighbors1)

yEmpty :: YColl
yEmpty = []


-----------------------
-- geometric utility --

intersect :: Line -> Line -> Maybe Point
intersect ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
  let
    a = (y1 - y0)
    b = - (x1 - x0)
    k = - (y1 - y0) * x0 + (x1 - x0) * y0
    a' = (y1' - y0')
    b' = - (x1' - x0')
    k' = - (y1' - y0') * x0' + (x1' - x0') * y0'
    divisor = a * b' - a' * b
    px = (b * k' - b' * k) / divisor
    py = (a' * k - a * k') / divisor
  in
  if divisor /= 0 && x0 <= px && px <= x1 && x0' <= px && px <= x1'
  then Just (px, py)
  else Nothing

intersectX :: X -> Line -> Maybe Point
intersectX x ((x0, y0), (x1, y1))
  | x < x0 || x1 < x = Nothing
  | otherwise = let y = y0 + ((y1 - y0) / (x1 - x0)) * (x - x0) in
                Just (x, y)

--------------------
-- main algorithm --

intersections :: [Line] -> [Point]
intersections lines =
  let
    initial = (xInitialize lines, yEmpty, [])
    finalResult = (`fix` initial) $ \loop (xcoll, ycoll, result) ->
      case xDropMin xcoll of
        Nothing -> result
        Just (el, xcoll') ->
          case el of
            LeftEnd (x, _) l ->
              let
                (ycoll', newNeighbors, newNonNeighbors) = yInsert x l ycoll
                xcoll'' = updateXColl x newNeighbors newNonNeighbors xcoll'
              in
              loop (xcoll'', ycoll', result)

            RightEnd (x, _) l ->
              let
                (ycoll', newNeighbors, newNonNeighbors) = yDelete x l ycoll
                xcoll'' = updateXColl x newNeighbors newNonNeighbors xcoll'
              in
              loop (xcoll'', ycoll', result)

            Intersect p@(x, _) l0 l1 ->
              let
                (ycoll', newNeighbors, newNonNeighbors) = ySwap x l0 l1 ycoll
                xcoll'' = updateXColl x newNeighbors newNonNeighbors xcoll'
              in
              loop (xcoll'', ycoll', p:result)
  in
  finalResult

updateXColl :: X -> [(Line, Line)] -> [(Line, Line)] -> XColl -> XColl
updateXColl x newNeighbors newNonNeighbors xcoll =
  let
    xcoll'  = foldr ($) xcoll (map (uncurry (updateNewNeighbor x)) newNeighbors)
    xcoll'' = foldr ($) xcoll' (map (uncurry (updateNewNonNeighbor x)) newNonNeighbors)
  in
  xcoll''

-- assume l0 is above l1 on x
updateNewNeighbor :: X -> Line -> Line -> XColl -> XColl
updateNewNeighbor x l0 l1 xcoll =
  case intersect l0 l1 of
    Nothing -> xcoll
    Just p@(x', _) | x < x' -> xInsert (Intersect p l0 l1) xcoll
                   | otherwise -> xcoll

-- assume l0 is above l1 on x
updateNewNonNeighbor :: X -> Line -> Line -> XColl -> XColl
updateNewNonNeighbor x l0 l1 xcoll =
  case intersect l0 l1 of
    Nothing -> xcoll
    Just p@(x', _) | x < x' -> xDelete (Intersect p l0 l1) xcoll
                   | otherwise  -> xcoll


-------------
-- testing --

spec :: Spec
spec =
  describe "intersections" $
    it "." $
      let lines = [ ((1, 3), (7, 4))
                  , ((2, 1), (8, 6))
                  , ((3, 5), (6, 2))
                  ]
      in
      intersections lines `shouldBe`
        [ (5.25, 3.7083333333333335)
        , (4.7272727272727275, 3.272727272727273)
        , (4.428571428571429, 3.5714285714285716)
        ]
