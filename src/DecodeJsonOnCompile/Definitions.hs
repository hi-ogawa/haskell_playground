{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DecodeJsonOnCompile.Definitions where

import GHC.Generics
import Data.Aeson
import Data.FileEmbed (embedFile)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import qualified Language.Haskell.TH.Syntax as THS

data Config = Config
  { database :: String
  , domain   :: String
  , port     :: Int
  , apiKey   :: String
  } deriving (Generic, Show, Eq)
instance ToJSON Config
instance FromJSON Config

-- NOTE: -XDeriveLift is available for template-haskell-2.11.0.0
instance THS.Lift Config where
 lift (Config a b c d) = [| Config $(THS.lift a) $(THS.lift b) $(THS.lift c) $(THS.lift d) |]

config0 :: FilePath -> IO (Either String Config)
config0 fp = eitherDecode <$> BL.readFile fp

config1 :: Either String Config
config1 = eitherDecode $ BL.fromStrict $(embedFile "resources/DecodeTest.json")
-- fp cannot be put as an argument because of GHC stage restriction
-- config1 fp = eitherDecode $ BL.fromStrict $(embedFile fp)

config2 :: THQ.QuasiQuoter
config2 = THQ.QuasiQuoter { THQ.quoteExp = quoteHExp
                           , THQ.quotePat  = undefined
                           , THQ.quoteType = undefined
                           , THQ.quoteDec  = undefined
                           }
  where
    s2bl :: String -> BL.ByteString
    s2bl = BL.fromStrict . TE.encodeUtf8 . T.pack
    quoteHExp :: String -> TH.ExpQ
    quoteHExp str = do
      case eitherDecode (s2bl str) :: Either String Config of
        Left msg -> fail $ "configFromJSON fails: " ++ msg
        Right settings -> THS.lift settings

config3 :: FilePath -> TH.ExpQ
config3 fp = do
  bl <- TH.runIO $ BL.readFile fp
  case eitherDecode bl :: Either String Config of
    Left msg -> fail $ "configFromJSONFile fails: " ++ msg
    Right settings -> THS.lift settings
