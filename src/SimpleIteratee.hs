module SimpleIteratee (spec) where

-- Motivation:
--  - simple implementation of `Iteratee` for learning purpose
--  - things to be noted:
--    - no monad transformation
--    - no `Exception` constructor
--    - `Nothing` as `EOF`
--    - no `Chunk`-ed input, so it can't wait to input coming
--    - no leave stream (`Yield` only leaves result `a`)
--  - things beyond scope:
--    - it doesn't accomodate IO monad

-- Conclusion:
--  - practical implementation of "interatcive input output monad"
--    - `Chunk`-ed input
--    - explicit `EOF` as input
--    - explicit `Exception` state
--    - `Enumerator` is only for programatic convention and
--       its type has no meaning "type-semantically"

-- Future Work:
--  - Monad instance for Iteratee might be "deriving"-able

-- Tips:
--  - quickcheck function testing:
--    - http://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html

import Control.Exception (assert)
import Control.Monad (join)
import qualified Test.Hspec as Hs
import qualified Test.Hspec.QuickCheck as HsQ
import Test.QuickCheck
import Test.QuickCheck.Function


data Iteratee s a =
    Continue (Maybe s -> Iteratee s a)
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

eCompose :: Enumerator s a -> Enumerator s a -> Enumerator s a
eCompose enum0 enum1 = enum1 . enum0


-------------------------------------------------------------
-- represent `list` in the language of iteratee/enumerator --

listFunction2Iteratee :: ([s] -> a) -> Iteratee s a
listFunction2Iteratee f =
  Continue $ \myb ->
    case myb of
      Nothing -> Yield (f [])
      Just s -> listFunction2Iteratee (f . (s:))

list2Enumerator :: [s] -> Enumerator s a
list2Enumerator ls = \it ->
  case it of
    Yield _ -> assert False undefined
    Continue f ->
      case ls of
        [] -> f Nothing
        h:tl -> list2Enumerator tl (f (Just h))


--------------
-- examples --


-- NOTE:
--  - iteratee version of implementation cannot be able to extracted from original one.
--    for example, `listFunction2Iteratee head` tries to consume every input at once
--    and that makes it impossible to compose iteratees like `ex0`.
iHead :: Iteratee a a
iHead = Continue $ \(Just s) -> Yield s

iTake :: Int -> Iteratee a [a]
iTake 0 = Yield []
iTake n = Continue $ \(Just s) -> (s:) `fmap` iTake (n - 1)

iDropAll :: Iteratee a ()
iDropAll = listFunction2Iteratee $ \_ -> ()

iSum_of_0th_and_5th :: Iteratee Int Int
iSum_of_0th_and_5th = do
  n <- iHead
  iTake 4
  m <- iHead
  iDropAll
  return (n + m)

eFilter :: (s -> Bool) -> Enumeratee s s a
eFilter p = \it ->
  case it of
    Yield _ -> Yield it
    Continue f -> Continue $
      \myb ->
        case myb of
          Nothing -> eFilter p (f Nothing)
          Just s | p s -> eFilter p (f (Just s))
                 | otherwise -> eFilter p it


----------
-- spec --

prop_iSum_of_0th_and_5th :: [Int] -> Property
prop_iSum_of_0th_and_5th ls =
  (length ls >= 6) ==> run (list2Enumerator ls iSum_of_0th_and_5th) === (ls !! 0) + (ls !! 5)

prop_listFunction2Iteratee_and_list2Enumerator :: Fun String Int -> String -> Property
prop_listFunction2Iteratee_and_list2Enumerator (Fun _ f) ls =
  run (list2Enumerator ls $ listFunction2Iteratee f) === f ls

-- NOTE: this is impossible to work since first `iHead` cannot wait second input
--       because of current simple `Iteratee` implementation
prop_eCompose :: [Int] -> [Int] -> Property
prop_eCompose l l' =
  let twoHeads :: Iteratee Int Int
      twoHeads = (+) <$> iHead <*> iHead
      ls = l ++ l' in
  (length ls >= 2) ==>
  run (list2Enumerator l `eCompose` list2Enumerator l' $ twoHeads) ===
    (ls !! 0) + (ls !! 1)

prop_eFilter :: Property
prop_eFilter =
  let it = join $ eFilter (> 5) (iHead <* iDropAll) in
  run (list2Enumerator [1..10::Int] it) === 6

spec :: Hs.Spec
spec = do
  Hs.describe "iSum_of_0th_and_5th" $ do
    HsQ.prop "." prop_iSum_of_0th_and_5th
  Hs.describe "listFunction2Iteratee, list2Enumerator" $ do
    HsQ.prop "." prop_listFunction2Iteratee_and_list2Enumerator
  Hs.describe "eCompose" $ do
    HsQ.prop "TODO" (const True prop_eCompose)
  Hs.describe "eFilter" $ HsQ.modifyMaxSuccess (const 1) $ do
    HsQ.prop "." prop_eFilter
