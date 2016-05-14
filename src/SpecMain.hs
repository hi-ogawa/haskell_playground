import Test.Hspec

import qualified Uva.P10043 as P10043
import qualified Uva.P10054 as P10054
import qualified Uva.P10065 as P10065
import qualified Uva.P10249 as P10249
import qualified Uva.P10032 as P10032
import qualified Uva.P10131 as P10131
import qualified Uva.P10154 as P10154
import qualified Uva.P10181 as P10181
import qualified Uva.P10041 as P10041
import qualified Uva.P10051 as P10051
import qualified Uva.P10003 as P10003
import qualified Uva.P116 as P116
import qualified IterateeWaitable as IterateeWaitable
import qualified IterateeNonWaitable as IterateeNonWaitable
import qualified Puzzles.Hooks2 as Hooks2

main :: IO ()
main = hspec $ do
  describe "P10043" $ P10043.spec
  describe "P10054" $ P10054.spec
  describe "P10065" $ P10065.spec
  describe "P10249" $ P10249.spec
  describe "P10032" $ P10032.spec
  describe "P10131" $ P10131.spec
  describe "P10154" $ P10154.spec
  describe "P10181" $ P10181.spec
  describe "P10041" $ P10041.spec
  describe "P10051" $ P10051.spec
  describe "P10003" $ P10003.spec
  describe "P116"   $ P116.spec
  describe "IterateeWaitable" $ IterateeWaitable.spec
  describe "IterateeNonWaitable" $ IterateeNonWaitable.spec
  describe "Hooks2" $ Hooks2.spec
