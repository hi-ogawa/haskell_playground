module Extentions.TemplateHaskellTupleGetter where

import Control.Monad
import Language.Haskell.TH

tupleGetter :: Int -> Int -> Q Exp
tupleGetter n i = do
  vars <- replicateM n (newName "_x") -- prefix underbar to avoid unused variable warning
  return $ LamE [TupP (map VarP vars)] (VarE (vars !! i))
