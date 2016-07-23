module Concur where

-- binary tree equivalece checking problem from go tour:
--   - https://tour.golang.org/concurrency/7
--   - https://golang.org/doc/play/tree.go

import Control.Concurrent
import Control.Concurrent.STM

import Test.Hspec

import qualified BinTree as B

eq :: Eq a => B.BinTree a -> B.BinTree a -> IO Bool
eq t0 t1 = do
  -- create two STM channel
  ch0 <- newTChanIO
  ch1 <- newTChanIO
  -- create two threads which put each trees' elements into channel in ascending order
  forkIO $ walker ch0 t0
  forkIO $ walker ch1 t1
  -- compare tree elements by extracting one by one from channel
  checker ch0 ch1

walker :: TChan (Maybe a) -> B.BinTree a -> IO ()
walker ch t = do
  trvs (atomically . writeTChan ch . Just) t
  atomically $ writeTChan ch Nothing
  where
    -- depth-first Traversable instance
    trvs :: Applicative f => (a -> f b) -> B.BinTree a -> f (B.BinTree b)
    trvs _ B.L = pure B.L
    trvs g (B.N x tl tr) = flip B.N <$> trvs g tl <*> g x <*> trvs g tr

checker :: Eq a => TChan (Maybe a) -> TChan (Maybe a) -> IO Bool
checker ch0 ch1 = do
  myb0 <- atomically $ readTChan ch0
  myb1 <- atomically $ readTChan ch1
  case (myb0, myb1) of
    (Nothing, Nothing) -> return True
    (Nothing, _      ) -> return False
    (_      , Nothing) -> return False
    (Just x , Just y ) | x == y    -> checker ch0 ch1
                       | otherwise -> return False


-------------
-- testing --

spec :: Spec
spec =
  describe "eq" $ do
    it "." $ do
      let t0 = B.N 3 (B.N 1 (B.N 1 B.L B.L)
                            (B.N 2 B.L B.L))
                     (B.N 8 (B.N 5 B.L B.L)
                            (B.N 13 B.L B.L))
          t1 = B.N 8 (B.N 3 (B.N 1 (B.N 1 B.L B.L)
                                   (B.N 2 B.L B.L))
                            (B.N 5 B.L B.L))
                     (B.N 13 B.L B.L)
      eq t0 t1 >>= (`shouldBe` True)
