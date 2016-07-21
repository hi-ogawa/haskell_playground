module BentleyOttmann (spec) where

import           Data.Function (fix)
import           Data.Maybe    (fromJust)
import           Prelude       hiding (lines)

import           Test.Hspec

import qualified BinTree       as BT


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

instance Ord XElem where
  el0 < el1 = xelem2x el0 < xelem2x el1
  el0 <= el1 = el0 < el1 || el0 == el1


---------------------------------------------
-- interfaces for X-structure, Y-structure --

type XColl = BT.BinTree XElem
type YColl = BT.BinTree Line

xInsert :: XElem -> XColl -> XColl
xInsert = BT.insert

xDropMin :: XColl -> Maybe (XElem, XColl)
xDropMin = BT.viewMin

xDelete :: XElem -> XColl -> XColl
xDelete = BT.delete

xInitialize :: [Line] -> XColl
xInitialize = BT.fromList . map (\l@(p, _) -> LeftEnd p l)

yOrder :: X -> Line -> Double
yOrder x l = snd . fromJust $ intersectX x l

-- detect neighbor changes along with each YColl operation
yInsert :: X -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
yInsert x l ycoll =
  let
    ycoll' = BT.insertWith (yOrder x) l ycoll
    (newNeighbors, newNonNeighbors) =
      case BT.neighborsWith (yOrder x) l ycoll' of
        (Nothing, Nothing) -> ([],                 [])
        (Just l0, Nothing) -> ([(l, l0)],          [])
        (Nothing, Just l1) -> ([(l1, l)],          [])
        (Just l0, Just l1) -> ([(l, l0), (l1, l)], [(l1, l0)])
  in
  (ycoll', newNeighbors, newNonNeighbors)

-- return `nonNeighbors` for consistent interface even though it's not necessary for Bentley-Ottmann algorithm
yDelete :: X -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
yDelete x l ycoll =
  let
    (newNeighbors, newNonNeighbors) =
      case BT.neighborsWith (yOrder x) l ycoll of
        (Nothing, Nothing) -> ([], [])
        (Just l0, Nothing) -> ([], [(l, l0)])
        (Nothing, Just l1) -> ([], [(l1, l)])
        (Just l0, Just l1) -> ([(l1, l0)], [(l1, l), (l, l0)])
    ycoll' = fromJust $ BT.deleteWith (yOrder x) l ycoll
  in
  (ycoll', newNeighbors, newNonNeighbors)

-- assume `lt` is above `lb` at the left of x
ySwap :: X -> Line -> Line -> YColl -> (YColl, [(Line, Line)], [(Line, Line)])
ySwap x lt lb ycoll =
  let
    (newNeighbors, newNonNeighbors) =
      case (fst $ BT.neighborsWith (yOrder x) lb ycoll, snd $ BT.neighborsWith (yOrder x) lt ycoll) of
        (Nothing, Nothing) -> ([(lb, lt)]          ,           [(lt, lb)])
        (Just l0, Nothing) -> ([(lb, lt), (lt, l0)],           [(lt, lb), (lb, l0)])
        (Nothing, Just l1) -> ([(lb, lt), (l1, lb)],           [(lt, lb), (l1, lt)])
        (Just l0, Just l1) -> ([(lb, lt), (l1, lb), (lt, l0)], [(lt, lb), (lb, l0), (l1, lt)])
    ycoll' = BT.swapWith (yOrder x) lt lb ycoll
  in
  (ycoll', newNeighbors, newNonNeighbors)

yEmpty :: YColl
yEmpty = BT.fromList []


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
  case l0 `intersect` l1 of
    Nothing -> xcoll
    Just p@(x', _) | x < x' -> xInsert (Intersect p l0 l1) xcoll
                   | otherwise -> xcoll

-- assume l0 is above l1 on x
updateNewNonNeighbor :: X -> Line -> Line -> XColl -> XColl
updateNewNonNeighbor x l0 l1 xcoll =
  case l0 `intersect` l1 of
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
