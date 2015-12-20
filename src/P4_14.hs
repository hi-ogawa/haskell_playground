module P4_14 where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Array as A
import qualified Data.Heap as H

type Heap  = H.Heap (Int, Int)
type Lists = A.Array Int [Int]
type Result = [Int]
type Er = ()
type CalcM = StateT Heap (ExceptT Er Identity)

solve :: Lists -> [Int]
solve sortedLists = undefined

init :: Lists -> (Lists, Heap)
init sortedLists = undefined

sub :: Lists -> CalcM (Lists)
sub sortedLists = undefined

step :: Lists -> Heap -> Either Result (Lists, Heap)
step sortedLists heap | (null heap || done sortedLists) = undefined
  where done :: Lists -> Bool
        done = undefined
