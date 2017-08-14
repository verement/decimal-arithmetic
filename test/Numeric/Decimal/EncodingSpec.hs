
{-# LANGUAGE OverloadedStrings #-}

module Numeric.Decimal.EncodingSpec (spec) where

import Test.Hspec
import Data.Binary

import Numeric.Decimal.Encoding

spec :: Spec
spec = do
  it "encodes (-7.50) correctly" $
    encode (read "-7.50" :: Decimal64) `shouldBe`
    "\xA2\x30\x00\x00\x00\x00\x03\xD0"
  it "decodes (-7.50) correctly" $
    (decode "\xA2\x30\x00\x00\x00\x00\x03\xD0" :: Decimal64) `shouldBe` (-7.50)

  it "decodes Infinity correctly" $
    (decode "\x78\xFF\xFF\xFF\xFF\xFF\xFF\xFF" :: Decimal64) `shouldSatisfy`
    \x -> isInfinite x && signum x == 1

-- prop> decode (encode x) == (x :: Decimal32)
-- prop> decode (encode x) == (x :: Decimal64)
-- prop> decode (encode x) == (x :: Decimal128)
