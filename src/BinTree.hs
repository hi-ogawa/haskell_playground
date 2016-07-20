module BinTree where

import           Test.Hspec

data BinTree a
  = N a (BinTree a) (BinTree a)
  | L
    deriving (Show, Eq)

null :: BinTree a -> Bool
null L = True
null _ = False

empty :: BinTree a
empty = L

fromList :: Ord a => [a] -> BinTree a
fromList = foldl (flip insert) L

insert :: Ord a => a -> BinTree a -> BinTree a
insert x L = N x L L
insert x (N x' t0 t1) | x <= x' = N x' (insert x t0) t1
                      | otherwise = N x' t0 (insert x t1)

delete :: Ord a => a -> BinTree a -> BinTree a
delete _ L = L
delete x (N x' t0 t1) | x < x' = N x' (delete x t0) t1
                      | x > x' = N x' t0 (delete x t1)
                      | x == x' = merge t0 t1
                      | otherwise = undefined
  where
    merge :: Ord a => BinTree a -> BinTree a -> BinTree a
    merge t L = t
    merge L t = t
    merge t (N _x _t0 _t1) = N _x t $ merge _t0 _t1


-- heap like interface (for BentleyOttmann x-structure)

viewMin :: Ord a => BinTree a -> Maybe (a, BinTree a)
viewMin t = f t >>= \x -> return (x, delete x t)
  where
    f :: BinTree a -> Maybe a
    f L = Nothing
    f (N x L _) = Just x
    f (N _ t' _) = f t'

spec :: Spec
spec =
  describe "fromList, insert, delete" $
    it "." $ do
      let t = fromList [3, 5, 2, 6 :: Int]
      t `shouldBe` N 3 (N 2 L L)
                       (N 5 L (N 6 L L))
      insert 4 t `shouldBe` N 3 (N 2 L L)
                                (N 5 (N 4 L L)
                                     (N 6 L L))
      delete 5 t `shouldBe` (N 3 (N 2 L L)
                                 (N 6 L L))
