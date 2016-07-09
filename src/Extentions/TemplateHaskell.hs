{-# LANGUAGE TemplateHaskell #-}
module Extentions.TemplateHaskell (basicSpec) where

-- Reference: https://wiki.haskell.org/Template_Haskell
-- NOTE:
-- - debug splices on ghci:
--   > :set -ddump-splices
--   > :load Extentions.TemplateHaskell

import Language.Haskell.TH
import qualified Extentions.TemplateHaskellAvoidStageRestriction as ASR
import Test.Hspec
import Extentions.TemplateHaskellTupleGetter (tupleGetter)

basicSpec :: Spec
basicSpec = do
  describe "[| ... |] :: Q Exp" $ do
    it "[| map (\\_ -> 0) [] |] is return (AppE (AppE (VarE 'map) ...)" $ do
      ex <- runQ [| map (\_ -> 0) [] |] :: IO Exp
      ex `shouldBe` AppE (AppE (VarE 'map) (LamE [WildP] (LitE (IntegerL 0)))) (ListE [])

  describe "\n'f and ''T" $ do
    it "'map :: Name" $ do
      'map `shouldBe` ('map :: Name)
    it "''String :: Name" $ do
      ''String `shouldBe` (''String :: Name)

  describe "\n$( ... ) where ... must be Q Exp " $ do
    it "$(return (LitE (StringL \"foo\"))) == \"foo\"" $ do
      $(return (LitE (StringL "foo"))) == "foo"
    it "$([| something |]) == something" $ do
      $([| 0 + 1 + 2 |]) == 0 + 1 + 2
    it "$x is $(x) where x :: Q Exp" $ do
      $((\x -> [| $x |]) (return (LitE (StringL "foo")))) == "foo"
      &&
      $((\x -> [| $(x) |]) (return (LitE (StringL "foo")))) == "foo"

  describe "\nQ Exp -> Q Exp" $ do
    it "\\e -> [| ($e, $e) |]" $ do
      $((\e -> [| ($e, $e) |]) (return (LitE (StringL "foo")))) == ("foo", "foo")

  describe "\nGHC stage restriction" $ do
    it "ASR.foo is okay but _foo is wrong" $ do
      let _foo = LitE (StringL "foo") :: Exp
      -- $(return _foo) == "foo" -- not allowed because of GHC stage restriction
      $(return ASR.foo) == "foo"

  describe "\ntupleGetter" $ do
    it "$(tupleGetter 7 4) (0, 1, 2, 3, 4, 5, 6) == 4" $ do
      $(tupleGetter 7 4) (0, 1, 2, 3, 4, 5, 6) == 4
