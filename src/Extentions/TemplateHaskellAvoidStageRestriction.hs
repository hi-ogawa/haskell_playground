{-# LANGUAGE TemplateHaskell #-}
module Extentions.TemplateHaskellAvoidStageRestriction (foo) where

import Language.Haskell.TH

foo :: Exp
foo = LitE (StringL "foo")
