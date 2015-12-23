{-# LANGUAGE TemplateHaskell #-}
module StringMatch where

import Control.Monad.State
import Control.Lens
import Control.Arrow

import qualified Data.ByteString.Char8 as B
import qualified Data.Array.Diff as A
import qualified Data.Sequence as S

data Move = K | C | I | D -- keep, change, insert, delete
                        deriving (Show)
type I = Int
data St = St {
  _arra :: A.Array (I, I) Int,
  _moves :: S.Seq Move
}
makeLenses ''St

type CompM a = StateT St IO a

-- difference penalties
changeP = 1
insertP = 1
deleteP = 1

-- examples:
-- λ> distance "fabulous" "ridiculous"
-- (5,fromList [I,I,C,C,C,K,K,K,K,K])

distance :: String -> String -> IO (Int, S.Seq Move)
distance s0 s1 =
  evalStateT (computation s0 s1) (St undefined undefined)

computation :: String -> String -> CompM (Int, S.Seq Move)
computation s0 s1 =
  do -- initialize states
     arra .= A.array indexR (map (id &&& f) $ A.range indexR)
     moves .= S.empty
     -- calculate distances
     mapM_ (step bs0 bs1) $ A.range ((0, 0), snd indexR)
     d <- use $ arra.to (A.! (snd indexR))
     -- backtrack choises we made
     iterateUntilM (backtrack bs0 bs1)
       (\(i, j) -> return . not $ i == -1 && j == -1)
       (snd indexR)
     ms <- use moves
     return (d, ms)
  where
    indexR = ((-1, -1), (length s0 - 1, length s1 - 1))
    f (-1, k) = k + 1
    f (i, -1) = i + 1
    bs0 = B.pack s0
    bs1 = B.pack s1

step :: B.ByteString -> B.ByteString -> (I, I) -> CompM ()
step s0 s1 (i, j) =
  do xx <- use $ arra.to (A.! (i - 1, j - 1))
     yy <- use $ arra.to (A.! (i    , j - 1))
     zz <- use $ arra.to (A.! (i - 1, j    ))
     let opt = minimum [
                 if (B.index s0 i) == (B.index s1 j) then xx else xx + changeP,
                 yy + deleteP,
                 zz + insertP
               ]
     arra %= (A.// [((i, j), opt)])

backtrack :: B.ByteString -> B.ByteString -> (I, I) -> CompM (I, I)
backtrack s0 s1 (i, j) =
  do arr <- use arra
     let op = arr A.! (i    , j    )
         xx = arr A.! (i - 1, j - 1)
         yy = arr A.! (i    , j - 1)
         zz = arr A.! (i - 1, j    )
     case () of
      _ | i == -1 ->
          do moves %= (I S.<|)
             return (i    , j - 1)
      _ | j == -1 ->
          do moves %= (D S.<|)
             return (i - 1, j    )
      _ | op == xx           && (B.index s0 i) == (B.index s1 j) ->
          do moves %= (K S.<|)
             return (i - 1, j - 1)
      _ | op == xx + changeP && (B.index s0 i) /= (B.index s1 j) ->
          do moves %= (C S.<|)
             return (i - 1, j - 1)
      _ | op == yy + deleteP  ->
          do moves %= (I S.<|)
             return (i    , j - 1)
      _ | op == zz + insertP ->
          do moves %= (D S.<|)
             return (i - 1, j    )


-- utility
iterateUntilM :: Monad m => (a -> m a) -> (a -> m Bool) -> a -> m [a]
iterateUntilM f p x =
  ifM (p x) (
    do x' <- f x
       ls <- iterateUntilM f p x'
       return $ x' : ls
  )
  (return [])

ifFM :: Monad m => (a -> m Bool) -> (a -> m b) -> (a -> m b) -> a -> m b
ifFM f g g' a = ifM (f a) (g a) (g' a)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb ma ma' = mb >>= (\b -> if b then ma else ma')
