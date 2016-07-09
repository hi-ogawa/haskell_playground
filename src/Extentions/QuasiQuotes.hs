{-# LANGUAGE QuasiQuotes #-}
module Extentions.QuasiQuotes (spec) where

import Extentions.QuasiQuoter
import Test.Hspec


spec :: Spec
spec =
  describe "\n[hexpr| ... |]" $ do
    it "[hexpr| ( 1 * (1 / 2)) |] == HBinOpE (HIntE 1) HMulO (... " $
      [hexpr| ( 1 * (1 / 2)) |] == HBinOpE (HIntE 1) HMulO (HBinOpE (HIntE 1) HDivO (HIntE 2))
