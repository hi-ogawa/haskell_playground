{-# LANGUAGE TemplateHaskell #-}

module DFSBFS where

-- control
import Control.Monad (filterM)
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Data.Functor

-- data structures
import qualified Data.Array.Diff as A
import qualified Data.Sequence as S

type Vertex = Int
type Graph = A.Array Vertex [Vertex]
type ParentRel = A.Array Vertex (Maybe Vertex)
data Mode = DFS | BFS
data VertexState = U | D | P -- undiscovered | discovered | processed
                           deriving (Eq)

data St = St {
    _graph :: Graph,
    _parent :: ParentRel,
    _vertexStates :: A.Array Vertex VertexState,
    _oldestAncestor :: A.Array Vertex Vertex,
    _buffer :: S.Seq (Vertex, Maybe Vertex),
    _mode :: Mode
  }
makeLenses ''St

type CompM a = StateT St IO a

-- procedure
-- assume connected and has at least one vertex
graphTraverse :: Graph -> Mode -> IO ParentRel
graphTraverse g m = evalStateT computation emptySt
  where
    emptySt = St undefined undefined undefined undefined undefined undefined
    computation =
      do initSt g m
         buffer %= (S.|> (head (verteces g), Nothing))
         steps
         use parent

initSt :: Graph -> Mode -> CompM ()
initSt g m =
  do graph .= g
     parent .= A.listArray (A.bounds g) (repeat Nothing)
     vertexStates .= A.listArray (A.bounds g) (repeat U)
     oldestAncestor .= A.array (A.bounds g) (map (id &&& id) (verteces g))
     buffer .= S.empty
     mode .= m

steps :: CompM ()
steps =
  do mvp <- pickNext
     case mvp of
      Nothing -> return ()
      Just (v, mp) ->
        do verst <- use vertexStates
           when (verst A.! v /= P) $ process v mp
           steps

-- NOTE: we don't need `D` (discovered) state
process :: Vertex -> Maybe Vertex -> CompM ()
process v mp =
  do vertexStates %= (A.// [(v, P)])
     parent %= (A.// [(v, mp)])
     (A.! v) <$> (use graph) >>=
       (
         filterM (\v' -> (/= P) . (A.! v') <$> (use vertexStates)) >=>
         mapM_ (\v' -> buffer %= (S.|> (v', Just v)))
       )

verteces :: Graph -> [Vertex]
verteces = A.range . A.bounds

pickNext :: CompM (Maybe (Vertex, Maybe Vertex))
pickNext =
  do m <- use mode
     buf <- use buffer
     case m of
      DFS ->
        case S.viewr buf of
         S.EmptyR -> return Nothing
         buf' S.:> v ->
           do buffer .= buf'
              return $ Just v
      BFS ->
        case S.viewl buf of
         S.EmptyL -> return Nothing
         v S.:< buf' ->
           do buffer .= buf'
              return $ Just v

-- examples
g0 :: Graph
g0 = A.array (0, 6) $ map (id &&& f) $ A.range (0, 6)
  where
    f :: Vertex -> [Vertex]
    f 0 = [1, 3, 5]
    f 1 = [0, 3]
    f 2 = [3, 4]
    f 3 = [0, 1, 2, 6]
    f 4 = [2, 6]
    f 5 = [0, 6]
    f 6 = [3, 4, 5]

-- λ> graphTraverse g0 DFS
-- array (0,6) [(0,Nothing),(1,Just 3),(2,Just 4),(3,Just 2),(4,Just 6),(5,Just 0),(6,Just 5)]
-- λ> graphTraverse g0 BFS
-- array (0,6) [(0,Nothing),(1,Just 0),(2,Just 3),(3,Just 0),(4,Just 2),(5,Just 0),(6,Just 3)]
