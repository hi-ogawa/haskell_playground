import Test.Hspec

import qualified Uva.P10043 as P10043
import qualified Uva.P10054 as P10054
import qualified Uva.P10065 as P10065

main :: IO ()
main = hspec $ do
  describe "P10043" $ P10043.spec
  describe "P10054" $ P10054.spec
  describe "P10065" $ P10065.spec
