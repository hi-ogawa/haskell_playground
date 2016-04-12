import Test.Hspec

import qualified Uva.P10043 as P10043
import qualified Uva.P10054 as P10054
import qualified Uva.P10065 as P10065
import qualified Uva.P10249 as P10249
import qualified Uva.P10032 as P10032
import qualified Uva.P10131 as P10131

main :: IO ()
main = hspec $ do
  describe "P10043" $ P10043.spec
  describe "P10054" $ P10054.spec
  describe "P10065" $ P10065.spec
  describe "P10249" $ P10249.spec
  describe "P10032" $ P10032.spec
  describe "P10131" $ P10131.spec
