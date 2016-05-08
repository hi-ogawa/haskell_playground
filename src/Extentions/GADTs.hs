{-# LANGUAGE GADTs #-}
module GADTs where

data Exp a where
  EInt  :: Int  -> Exp Int
  EBool :: Bool -> Exp Bool
  EAdd  :: Exp Int -> Exp Int -> Exp Int
  EEq   :: (Eq a) => Exp a -> Exp a -> Exp Bool
  EIf   :: Exp Bool -> Exp a -> Exp a -> Exp a
  EAbs  :: (Exp a -> Exp b) -> Exp (a -> b)
  EApp  :: Exp (a -> b) -> Exp a -> Exp b
  EVar  :: String -> Exp a

-- pros:
--   - you can borrow haskell's type check for restricting a term of expression e.g.

-- λ> :t EAdd (EInt 1) (EInt 2)
-- EAdd (EInt 1) (EInt 2) :: Exp Int
-- λ> :t EAdd (EInt 1) (EBool True)
-- <interactive>:1:16-25:
--     Couldn't match type ‘Bool’ with ‘Int’
--     Expected type: Exp Int
--       Actual type: Exp Bool
--     In the second argument of ‘EAdd’, namely ‘(EBool True)’
--     In the expression: EAdd (EInt 1) (EBool True)


-- if you try to do the same thing with "oridinally" algebraic data type:

data Exp' a
  = EInt' Int
  | EBool' Bool
  | EAdd' (Exp' Int) (Exp' Int)
  | EIf' (Exp' Bool) (Exp' a) (Exp' a)

-- type is not imposed:

-- λ> :t EInt' 10
-- EInt' 10 :: Exp' a
-- λ> :t EAdd' (EInt' 1) (EBool' True)
-- EAdd' (EInt' 1) (EBool' True) :: Exp a

-- so this is nothing but:

data Exp''
  = EInt'' Int
  | EBool'' Bool
  | EAdd'' Exp'' Exp''
  | EIf'' Exp'' Exp'' Exp''
