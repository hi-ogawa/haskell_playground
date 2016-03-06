module Main where

import GHC.Environment (getFullArgs)
import Control.Exception (assert)

import qualified Uva.P10054 as P10054
import qualified Uva.P10065 as P10065

main :: IO ()
main =
  do args <- getFullArgs
     case head args of
       "10054" -> P10054.main
       "10065" -> P10065.main
       _ -> assert False undefined
