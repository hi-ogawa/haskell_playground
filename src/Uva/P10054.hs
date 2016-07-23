{-# LANGUAGE TupleSections #-}

module Uva.P10054 (main, spec) where

import Control.Exception (assert)

import Test.Hspec

import Control.Monad.State
import Control.Arrow
import Control.Lens
import System.IO.Unsafe (unsafePerformIO) -- for debug

import Control.Monad.Writer (Writer, execWriter, tell)
import Text.Parsec

import Data.List (intercalate)
import qualified Data.Array.Diff as A
import qualified Data.Sequence as S

type Color = Int
type Graph = A.Array (Color, Color) Int
type Path = S.Seq Color

---------------
-- main -------
---------------

type MockO = Writer String

putStrMock :: String -> MockO ()
putStrMock = tell

main :: IO ()
main = putStr . mainPure =<< getContents

mainPure :: String -> String
mainPure = execWriter . runParserT pMain () ""

pMain :: ParsecT String () MockO ()
pMain =
  do nCases <- pNum; newline
     count nCases pGraph >>= (
       zip (fix (\f i -> i : f (i + 1)) 1) >>>
       map (uncurry solve) >>>
       intercalate "\n" >>>
       lift . putStrMock
       )

pGraph :: ParsecT String () MockO Graph
pGraph =
  do nEdges <- pNum; newline
     edges <- count nEdges (
       do n0 <- pNum; space
          -- don't consume the last newline of whole input
          n1 <- pNum; (optional newline)
          return (n0, n1)
       )
     return $ A.accumArray (\dup _ -> dup + 1) 0 graphBnds $
       map (, undefined) $ edges >>= (\(s, e) -> [(s, e), (e, s)])

pNum :: Monad m => Read a => Num a => ParsecT String () m a
pNum = read `fmap` many digit

solve :: Int -> Graph -> String
solve i g =
  "Case #" ++ show i ++ "\n" ++
  if eulerian g
  then showPath $ eulerTour g
  else "some beads may be lost"
  ++ "\n"

showPath :: Path -> String
showPath path =
  case S.viewl path of
    hd S.:< tl -> show hd ++ " " ++ f tl
    _ -> assert False undefined
  where
    f _path =
      case S.viewl _path of
       hd S.:< tl -> show hd ++ "\n" ++
                     if S.null tl
                     then ""
                     else show hd ++ " " ++ f tl
       _ -> assert False undefined

-- global readonly vars
graphBnds :: ((Color, Color), (Color, Color))
graphBnds = ((1, 1), (50, 50))
colorBnds :: (Color, Color)
colorBnds = (1, 50)
colors :: [Color]
colors = A.range colorBnds

---------------
-- utilities --
---------------

degree :: Graph -> Color -> Int
degree g c = sum $ map (\c' -> g A.! (c, c')) colors

_insertEdge :: (Color, Color) -> Graph -> Graph
_insertEdge (c, c') g =
  A.accum (\dup _ -> dup + 1) g $
    map (, undefined) [(c, c'), (c', c)]

deleteEdge :: (Color, Color) -> Graph -> Graph
deleteEdge (c, c') g =
  A.accum (\dup _ -> dup - 1) g $
    map (, undefined) [(c, c'), (c', c)]

-----------------------------
-- euler tour construction --
-----------------------------

type CompM a = StateT (Graph, Path) IO a

-- O(n^2 * m^2)
eulerTour :: Graph -> Path
eulerTour g =
  eval (
    do _1 .= g
       _2 .= S.empty
       eulerTour' $ head (filter ((0 <) . degree g) colors)
       use _2
    )
  where
    eval :: CompM a -> a
    eval cmp = unsafePerformIO $ evalStateT cmp (undefined, undefined)

eulerTour' :: Color -> CompM ()
eulerTour' c =
  do _2 %= (S.|> c)
     g <- use _1
     unless (degree g c == 0) (
       do let c' = chooseNextColor g c
          _1 %= deleteEdge (c, c')
          eulerTour' c'
       )

-- O(n^2 * m)
chooseNextColor :: Graph -> Color -> Color
chooseNextColor g c =
  colors & (
    filter (\c' -> g A.! (c, c') > 0) >>>
    filter (\c' ->
      let g' = deleteEdge (c, c') g in
      connectedN g' == 0 ||
      (connectedN g' == 1 && degree g' c' > 0)
      ) >>>
    head
    )

--------------------
-- eulerian check --
--------------------

eulerian :: Graph -> Bool
eulerian g = connectedN g <= 1 && (and . map (even . degree g)) colors

type CompM' a =
  StateT (
    Graph,               -- graph
    A.Array Color Bool,  -- visited flag
    Int                  -- number of connected components
    ) IO a

-- number of connected components (O(m * n))
connectedN :: Graph -> Int
connectedN g = eval cmp
  where
    eval :: CompM' a -> a
    eval _cmp = unsafePerformIO $ evalStateT _cmp (undefined, undefined, undefined)
    cmp :: CompM' Int
    cmp =
      do _1 .= g
         _2 .= nonZeroDegrees
         _3 .= 0
         mapM_ traverseComponent colors
         use _3
    nonZeroDegrees :: A.Array Color Bool
    nonZeroDegrees = A.array colorBnds $ map (id &&& (0 ==) . degree g) colors

traverseComponent :: Color -> CompM' ()
traverseComponent c =
  do cIsVisited <- use $ _2.to (A.! c)
     unless (cIsVisited) (
       do _3 %= (+1)
          traverseGraph c
       )

traverseGraph :: Color -> CompM' ()
traverseGraph c =
  do _2 %= (A.// [(c, True)])
     g <- use _1
     vtd <- use _2
     colors & (
       filter (\c' -> g A.! (c, c') > 0) >>>
       filter (not . (vtd A.!)) >>>
       mapM_ traverseGraph
       )


----------
-- spec --

spec :: Spec
spec = do
  describe "mainPure" $ do
    it "." $ do
      input <- readFile "./resources/UVA10054.input"
      output <- readFile "./resources/UVA10054.output"
      mainPure input `shouldBe` output
