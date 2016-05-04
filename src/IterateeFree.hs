module IterateeFree where

-- TODO:
--  - fill monad definition
--  - add examples

-- note this definition is different from the original one from package free.
data Free f a = Free (f (Free f a))

data IterateeF s a x
  = C (s -> x)
  | Y a

type Iteratee s a = Free (IterateeF s a)
