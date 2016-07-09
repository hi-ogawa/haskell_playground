{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DecodeJsonOnCompile.Usage where

-- NOTE: need to separate modules because of GHC stage restriction
import DecodeJsonOnCompile.Definitions
import Test.Hspec

spec :: Spec
spec = do
  describe "json data conversion" $ do
    it "runtime decode with runtime file read" $ do
      Right c <- config0 "resources/DecodeTest.json"
      c `shouldBe`
        Config { database = "host=192.16..."
               , domain = "super.com"
               , port = 8080
               , apiKey = "asdfjkl;" }

    it "runtime decode with compile time file read" $
      config1 `shouldBe` Right (
        Config { database = "host=192.16..."
               , domain = "super.com"
               , port = 8080
               , apiKey = "asdfjkl;" }
        )

    it "compile time decode as quasiquoter" $
      [config2| {
        "database": "host=...",
        "domain":   "super.com",
        "port":     8080,
        "apiKey":   "asdfjkl;"
      }|] `shouldBe` Config { database = "host=..."
                            , domain = "super.com"
                            , port = 8080
                            , apiKey = "asdfjkl;"
                            }

    it "compile time decode with compile time file read" $
      $(config3 "resources/DecodeTest.json") `shouldBe`
        Config { database = "host=192.16..."
               , domain = "super.com"
               , port = 8080
               , apiKey = "asdfjkl;" }
