{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module DataKinds where

-- reference:
--   - https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat
  = O
  | S Nat
    deriving (Show)

data Vec :: * -> Nat -> * where
  Nil :: Vec a 'O
  Cons :: a -> Vec a n -> Vec a ('S n)

-- `GADTs` is used for

--   data ... where
--     ...


-- `KindSignatures` is used for

--   Vec :: * -> Nat -> *


-- `DataKinds` is used for

--   Vec a 'O
--   Vec a ('S n)

--  note:
--   - `Nat` is promoted to be "kind"
--   - quoted version of the constructor (`S` and `O`) is prooted to be "type" of the kind `Nat`


hd :: Vec a ('S n) -> a
hd v =
  case v of
    Cons x _  -> x

-- TypeOperators is used for `add`
add :: Nat -> Nat -> Nat
add m n =
  case m of
    O    -> n
    S m' -> S (add m' n)

-- app :: Vec a m -> Vec a n -> Vec a (m `add` n)
-- app v0 v1 =
--   case v0 of
--     Nil -> v1                          -- => TYPE ERROR: Could not deduce (n ~ add 'O n)
--     Cons x v0' -> Cons x (app v0' v1)  -- => TYPE ERROR: Couldn't match type ‘'S’ with ‘add ('S n1)’


main :: IO ()
main = do
  print $ hd (Cons True Nil) -- => True
  -- print $ hd Nil -- => TYPE ERROR

  print $ add (S (S O)) (S (S (S O)))
