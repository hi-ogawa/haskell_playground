{-# LANGUAGE TupleSections #-}

module Uva.P10054 where

import Control.Exception (assert)

-- NOTE:
--   - on my local ghc 7.10, (<*) and (<$>) loaded via GHC.Base.
--   - on travis ci ghc 7.8, they are not.
import Control.Applicative ((<*))
import Data.Functor ((<$>))

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
  do nCases <- pNum <* newline
     count nCases pGraph >>= (
       zip (fix (\f i -> i : f (i + 1)) 1) >>>
       map (uncurry solve) >>>
       intercalate "\n" >>>
       lift . putStrMock
       )

pGraph :: ParsecT String () MockO Graph
pGraph =
  do nEdges <- pNum <* newline
     edges <- count nEdges (
       do n0 <- pNum <* space
          -- don't consume the last newline of whole input
          n1 <- pNum <* (optional newline)
          return (n0, n1)
       )
     return $ A.accumArray (\dup _ -> dup + 1) 0 graphBnds $
       map (, undefined) $ edges >>= (\(s, e) -> [(s, e), (e, s)])

pNum :: Monad m => Read a => Num a => ParsecT String () m a
pNum = read <$> many digit

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

insertEdge :: (Color, Color) -> Graph -> Graph
insertEdge (c, c') g =
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
    -- NOTE: you need to count the vertex c' as one component after removing (c, c')
    --       this approach doesn't look good but it works.
    filter (\c' -> connected (insertEdge (c', c') (deleteEdge (c, c') g))) >>>
    head
    )

--------------------
-- eulerian check --
--------------------

eulerian :: Graph -> Bool
eulerian g = connected g && (and . map (even . degree g)) colors

type CompM' a =
  StateT (
    Graph,               -- graph
    A.Array Color Bool,  -- visited flag
    Int                  -- number of connected components
    ) IO a

-- check if non-zero degree verteces are connected (O(m * n))
connected :: Graph -> Bool
connected g = eval cmp
  where
    eval :: CompM' a -> a
    eval _cmp = unsafePerformIO $ evalStateT _cmp (undefined, undefined, undefined)
    cmp :: CompM' Bool
    cmp =
      do _1 .= g
         _2 .= nonZeroDegrees
         _3 .= 0
         mapM_ traverseComponent colors
         use $ _3.to (<= 1)
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
