module Main where

import GHC.Environment (getFullArgs)
import Control.Exception (assert)

import qualified Uva.P10054 as P10054
import qualified Uva.P10065 as P10065

-- example usage:
--
--   # build executable
--   $ cabal build
--
--   # see output
--   $ ./dist/build/main/main 10065 < ./resources/UVA10065.input
--
--   # directly check diff using bash syntax: http://askubuntu.com/questions/229447/how-do-i-diff-the-output-of-two-commands
--   $ diff <(./dist/build/main/main 10065 < ./resources/UVA10065.input) <(cat ./resources/UVA10065.output)

main :: IO ()
main =
  do args <- getFullArgs
     case head args of
       "10054" -> P10054.main
       "10065" -> P10065.main
       _ -> assert False undefined
