module Main where

import GHC.Environment (getFullArgs)
import Control.Exception (assert)

import qualified UVA10054 as UVA10054
import qualified Uva.P10065 as P10065

main :: IO ()
main =
  do args <- getFullArgs
     case head args of
       "10054" -> P10065.main
       "10065" -> UVA10054.main
       _ -> assert False undefined


