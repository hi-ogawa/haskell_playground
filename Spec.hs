import Test.Hspec

import qualified Uva.P10043
import qualified Uva.P10054
import qualified Uva.P10065
import qualified Uva.P10249
import qualified Uva.P10032
import qualified Uva.P10131
import qualified Uva.P10154
import qualified Uva.P10181
import qualified Uva.P10041
import qualified Uva.P10051
import qualified Uva.P10003
import qualified Uva.P116
import qualified IterateeWaitable
import qualified IterateeNonWaitable
import qualified Puzzles.Hooks2
import qualified Uva.P10261
import qualified Uva.P10026
import qualified Uva.P10152
import qualified Kopt
import qualified Uva.P10084
import qualified FloatingPointNumber
import qualified StringConversions
import qualified Extentions.TemplateHaskell as ETH
import qualified Extentions.QuasiQuotes as EQQ
import qualified DecodeJsonOnCompile.Usage
import qualified BentleyOttmann
import qualified BinTree

main :: IO ()
main = hspec $ do
  describe "P10043" $ Uva.P10043.spec
  describe "P10054" $ Uva.P10054.spec
  describe "P10065" $ Uva.P10065.spec
  describe "P10249" $ Uva.P10249.spec
  describe "P10032" $ Uva.P10032.spec
  describe "P10131" $ Uva.P10131.spec
  describe "P10154" $ Uva.P10154.spec
  describe "P10181" $ Uva.P10181.spec
  describe "P10041" $ Uva.P10041.spec
  describe "P10051" $ Uva.P10051.spec
  describe "P10003" $ Uva.P10003.spec
  describe "P116"   $ Uva.P116.spec
  describe "IterateeWaitable" $ IterateeWaitable.spec
  describe "IterateeNonWaitable" $ IterateeNonWaitable.spec
  describe "Hooks2" $ Puzzles.Hooks2.spec
  describe "P10261" $ Uva.P10261.spec
  describe "P10026" $ Uva.P10026.spec
  describe "P10152" $ Uva.P10152.spec
  describe "Kopt" $ Kopt.spec
  describe "P10084" $ Uva.P10084.spec
  describe "FloatingPointNumber" $ FloatingPointNumber.spec
  describe "StringConversions" $ StringConversions.spec
  describe "TemplateHaskell" $ ETH.basicSpec
  describe "QuasiQuotes" $ EQQ.spec
  describe "DecodeJsonOnCompile.Usage" $ DecodeJsonOnCompile.Usage.spec
  describe "BentleyOttmann" $ BentleyOttmann.spec
  describe "BinTree" $ BinTree.spec
