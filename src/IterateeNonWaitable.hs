module IterateeNonWaitable (spec) where

-- NOTE:
--  - enumerator composition doesn't work for non waitable iteratee defintion.
--  - impossible to implement `listFunction2Iteratee` since `Nothing` input did good ad-hocly

import Control.Exception (assert)
import Control.Monad (join)
import qualified Test.Hspec as Hs
import qualified Test.Hspec.QuickCheck as HsQ
import Test.QuickCheck


data Iteratee s a =
    Continue (s -> Iteratee s a)
  | Yield a

type Enumerator s a = Iteratee s a -> Iteratee s a
type Enumeratee sFrom sTo a = Iteratee sTo a -> Iteratee sFrom (Iteratee sTo a)

instance Functor (Iteratee s) where
  g `fmap` (Continue f) = Continue $ \s -> g `fmap` f s
  g `fmap` (Yield a) = Yield $ g a

instance Applicative (Iteratee s) where
  pure a = return a
  mg <*> ma = do g <- mg; a <- ma; return (g a)

instance Monad (Iteratee s) where
  return a = Yield a
  (Yield a) >>= kleisli_g = kleisli_g a
  (Continue f) >>= kleisli_g = Continue $ \s -> (f s) >>= kleisli_g


-----------------------------------
-- iteratee/enumerator interface --

run :: Iteratee s a -> a
run (Yield a) = a
run _ = assert False undefined


----------------------------------
-- represent list as enumerator --

list2Enumerator :: [s] -> Enumerator s a
list2Enumerator ls = \it ->
  case it of
    Yield _ -> it
    Continue f ->
      case ls of
        [] -> assert False undefined
        h:tl -> list2Enumerator tl (f h)


--------------
-- examples --

iHead :: Iteratee a a
iHead = Continue $ \s -> Yield s

iTake :: Int -> Iteratee a [a]
iTake 0 = Yield []
iTake n = Continue $ \s -> (s:) `fmap` iTake (n - 1)

iSum_of_0th_and_5th :: Iteratee Int Int
iSum_of_0th_and_5th = do
  n <- iHead
  iTake 4
  m <- iHead
  return (n + m)

eFilter :: (s -> Bool) -> Enumeratee s s a
eFilter p = \it ->
  case it of
    Yield _ -> Yield it
    Continue f -> Continue $ \s ->
      if p s
      then eFilter p (f s)
      else eFilter p it


----------
-- spec --

prop_iSum_of_0th_and_5th :: [Int] -> Property
prop_iSum_of_0th_and_5th ls =
  (length ls >= 6) ==> run (list2Enumerator ls iSum_of_0th_and_5th) === (ls !! 0) + (ls !! 5)

prop_eFilter :: Property
prop_eFilter =
  let it = join $ eFilter (> 5) (iHead) in
  run (list2Enumerator [1..10::Int] it) === 6

spec :: Hs.Spec
spec = do
  Hs.describe "iSum_of_0th_and_5th" $ do
    HsQ.prop "." prop_iSum_of_0th_and_5th
  Hs.describe "eFilter" $ HsQ.modifyMaxSuccess (const 1) $ do
    HsQ.prop "." prop_eFilter
