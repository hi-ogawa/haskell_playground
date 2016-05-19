{-# LANGUAGE TypeFamilies #-}
module TypeFamilies where

-- Reference:
--  - detailed rules:
--    - https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/type-families.html
--  - use case:
--    - https://wiki.haskell.org/GHC/Indexed_types


-- Analogy:

-- Same implementation for any `a`  <->  Implementation is changeable for each type instance

-- parametric polymorphic function  <->  ad-hoc polymorphic method
--
--   id :: a -> a                          class Monad m where
--   id x = x                                 return :: a -> m a ...
--
--                                         instance Monad (Maybe) where
--                                            return = Just ...


-- parametrized list type           <->  type family
--
--   data List a                           data family T a
--     = Nil                               data instance T int = T1 Int | T2 Bool
--     | Cons a (List a)                   data instance T Char = TC Bool


class XXClass m where
  data XXData m
  xx :: a -> m a
