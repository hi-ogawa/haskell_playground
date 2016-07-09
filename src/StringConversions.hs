{-# LANGUAGE OverloadedStrings #-}
module StringConversions where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Test.Hspec

spec :: Spec
spec = do
  let s   = "hello" :: String
      b   = "hello" :: B.ByteString
      bc  = "hello" :: BC.ByteString
      bl  = "hello" :: BL.ByteString
      blc = "hello" :: BLC.ByteString
      t   = "hello" :: T.Text
      tl  = "hello" :: TL.Text
  describe "string-like types conversions" $ do
    describe "encodeUtf8, decodeUtf8" $ do
      it "B   -> T"   $ TE.decodeUtf8 b == t
      it "T   -> B"   $ TE.encodeUtf8 t == b
    describe "fromStrict, toStrict" $ do
      it "B   -> BL"  $ BL.fromStrict b == bl
      it "BL  -> B"   $ BL.toStrict bl == b
      it "BLC -> BC"  $ BLC.fromStrict bc == blc
      it "BC  -> BLC" $ BLC.toStrict blc == bc
      it "TL  -> T"   $ TL.fromStrict t == tl
      it "T   -> TL"  $ TL.toStrict tl == t
    describe "pack, unpack" $ do
      it "S   -> T"   $ T.pack s == t
      it "S   -> TL"  $ TL.pack s == tl
      it "S   -> BC"  $ BC.pack s == bc
      it "S   -> BLC" $ BLC.pack s == blc
      it "T   -> S"   $ T.unpack t == s
      it "TL  -> S"   $ TL.unpack tl == s
      it "BC  -> S"   $ BC.unpack bc == s
      it "BLC -> S"   $ BLC.unpack blc == s
