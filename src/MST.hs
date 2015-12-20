{-# LANGUAGE TemplateHaskell #-}

module MST where

-- control
import Control.Monad.State
import Control.Lens
import Control.Arrow

-- data structures
import qualified Data.Array.Diff as A
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.List (minimumBy, intersect)
import Data.Ord (comparing)

-- data definition
type Vertex = Int
type Weight = Float
type WGraphM = A.Array (Vertex, Vertex) Weight
type WGraphL = M.Map Vertex [(Vertex, Weight)]

-- computation data definition
type Distances = A.Array Vertex (Maybe Weight)
type Parents = A.Array Vertex (Maybe Vertex)
type Visited = A.Array Vertex Bool

data St = St {
    _distances :: Distances,
    _parentsRel :: Parents,
    _graph :: WGraphM,
    _visited :: Visited
  }
makeLenses ''St
type CompM a = StateT St IO a


-- NOTE: assueme graph is connected and has at least one vertex
primMST :: WGraphM -> IO Parents
primMST g = fmap fst $ runStateT computation mockSt
  where computation :: CompM Parents
        computation =
          do initSt g
             steps (fst . head $ A.indices g) Nothing
             use parentsRel
        mockSt = St undefined undefined undefined undefined

verteces :: WGraphM -> [Vertex]
verteces g = A.range (st, en)
  where ((st, _), (en, _)) = A.bounds g

initSt :: WGraphM -> CompM ()
initSt g =
  do graph .= g
     parentsRel .= (A.listArray (st, en) $ repeat Nothing)
     distances .= (A.listArray (st, en) $ repeat Nothing)
     visited .= (A.listArray (st, en) $ repeat False)
  where
    (st, en) = g & (verteces >>> (head &&& last))

steps :: Vertex -> Maybe Vertex -> CompM ()
steps v p =
  do visited %= (A.// [(v, True)])
     updateDistances v
     mnv <- takeNearest
     case mnv of
      Just nv -> steps nv (Just v)
      Nothing -> return ()

updateDistances :: Vertex -> CompM ()
updateDistances v =
  do g <- use graph
     visitedA <- use visited
     distancesA <- use distances
     verteces g &
       (
         filter (\v' -> not (visitedA A.! v')) >>>
         flip forM_
         (\v' ->
           let d = g A.! (v, v') in
           case distancesA A.! v' of
            Just d' | d' < d -> return ()
            _ ->
              do parentsRel %= (A.// [(v', Just v)])
                 distances %= (A.// [(v', Just d)])
         )
       )

takeNearest :: CompM (Maybe Vertex)
takeNearest =
  do g <- use graph
     visitedA <- use visited
     distancesA <- use distances
     verteces g &
       (
         filter (not . (visitedA A.!)) >>>
         filter (isJust . (distancesA A.!)) >>>
         (\vs ->
           if null vs
           then Nothing
           else Just $ minimumBy (comparing (fromJust . (distancesA A.!))) vs
         ) >>> return
       )

-- examples
g0 :: WGraphM
g0 = A.array ((0, 0), (4, 4)) $
       A.range ((0, 0), (4, 4)) &
         ((id &&& map f) >>> uncurry zip)
  where
    f :: (Vertex, Vertex) -> Weight
    f t =
      [
        [  0,  3,  2,  5,  6],
        [  3,  0,  2,  4,  4],
        [  2,  2,  0,1.5,  3],
        [  5,  4,1.5,  0,  1],
        [  6,  4,  3,  1,  0]
      ] !! (fst t) !! (snd t)

-- "
-- 0      1

--    2

--      3  4
-- "
