module StateTFix where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

-- TODO: does the order of transformer matter for definition of `mfix` ?
--       A. it doesn't matter.
type CalcM = StateT String (ExceptT Int Identity)
type CalcM' = ExceptT Int (StateT String Identity)

-- NOTE: `mfix` doesn't update states while looping (why?)
-- NOTE: the definition of mfix for StateT is bullshit (why?)
testF :: CalcM ()
testF = mfix f

testG :: CalcM' ()
testG = mfix g

f :: () -> CalcM ()
f () =
  do x <- get
     case x of
      []  -> throwError 0
      h:t -> put t

g :: () -> CalcM' ()
g () =
  do x <- get
     case x of
      []  -> throwError 0
      h:t -> put t


-- TODO: I thought mfix f == f' why??
f' :: CalcM ()
f' =
  do x <- get
     case x of
      []  -> throwError 0
      h:t ->
        do put t
           f'

-- f :: Calc Int
-- f =
--   do x <- get
--      return $ length x

-- g :: Either Int (Calc Int)
-- g = undefined

-- i -> State String i
-- i -> Maybe i
