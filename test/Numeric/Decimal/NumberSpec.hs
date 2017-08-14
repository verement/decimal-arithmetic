
module Numeric.Decimal.NumberSpec (spec) where

import Arbitrary ()

import Test.Hspec
import Test.QuickCheck
import Numeric.Decimal

spec :: Spec
spec = do
  describe "Read" $ do
    it "reads with Just correctly (positive)" $
      (read "Just 123" :: Maybe GeneralDecimal) `shouldBe` Just 123
    it "reads with Just correctly (negative)" $
      (read "Just (-12.0)" :: Maybe GeneralDecimal) `shouldBe` Just (-12)

  describe "Ord" $ do
    it "satisfies (>) invariant" $
      property $ \x y ->
      x > y ==> max x y == x && max y x == (x :: BasicDecimal)
    it "satisfies (<) invariant" $
      property $ \x y ->
      x < y ==> min x y == x && min y x == (x :: BasicDecimal)

    it "satisfies `max` invariant with respect to 1st arg" $
      property $ \x y ->
      max x y == x ==> x >= (y :: BasicDecimal)
    it "satisfies `max` invariant with respect to 2nd arg" $
      property $ \x y ->
      max x y == y ==> y >= (x :: BasicDecimal)
    it "satisfies `min` invariant with respect to 1st arg" $
      property $ \x y ->
      min x y == x ==> x <= (y :: BasicDecimal)
    it "satisfies `min` invariant with respect to 2nd arg" $
      property $ \x y ->
      min x y == y ==> y <= (x :: BasicDecimal)

  describe "Enum" $ do
    it "enumerates `enumFromTo` precisely" $
      ([1.7 .. 5.7] :: [BasicDecimal]) `shouldBe` [1.7, 2.7, 3.7, 4.7, 5.7]

    it "enumerates `enumFromThenTo` precisely (ascending)" $
      ([0, 0.1 .. 2] :: [BasicDecimal]) `shouldBe`
      [  0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
       1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
    it "enumerates `enumFromThenTo` precisely (descending)" $
      ([2, 1.9 .. 0] :: [BasicDecimal]) `shouldBe`
      [  2, 1.9, 1.8, 1.7, 1.6, 1.5, 1.4, 1.3, 1.2, 1.1,
       1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0]

  describe "Num" $ do
    it "satisfies relation between (+) and (*)" $
      property $ \x ->
      x + x == x * (2 :: GeneralDecimal)
    it "satisfies relation between (-) and 0" $
      property $ \x ->
      isFinite x ==> x - x == (0 :: BasicDecimal)
    it "satisfies relation between (+) and `negate`" $
      property $ \x ->
      isFinite x ==> x + negate x == (0 :: BasicDecimal)
    it "satisfies relation between `abs` and 0" $
      property $ \x ->
      abs x >= (0 :: GeneralDecimal)
    it "satisfies relation between `abs` and `signum`" $
      property $ \x ->
      abs x * signum x == (x :: GeneralDecimal)

  describe "Fractional" $ do
    context "RoundHalfUp" $ do
      it "properly rounds (down)" $
        (4.14 :: Decimal P2 RoundHalfUp)   `shouldBe` 4.1
      it "properly rounds (up)" $
        (4.15 :: Decimal P2 RoundHalfUp)   `shouldBe` 4.2
    context "RoundHalfDown" $
      it "properly rounds" $
        (4.15 :: Decimal P2 RoundHalfDown) `shouldBe` 4.1
    context "RoundHalfEven" $ do
      it "properly rounds 1 (up)" $
        (4.15 :: Decimal P2 RoundHalfEven) `shouldBe` 4.2
      it "properly rounds 1 (down)" $
        (4.25 :: Decimal P2 RoundHalfEven) `shouldBe` 4.2
      it "properly rounds 2 (up)" $
        (4.35 :: Decimal P2 RoundHalfEven) `shouldBe` 4.4
      it "properly rounds 2 (down)" $
        (4.45 :: Decimal P2 RoundHalfEven) `shouldBe` 4.4

  describe "RealFrac" $ do
    it "satisfies `properFraction` invariant 1" $
      property $ \x ->
      let (n,f) = properFraction (x :: BasicDecimal) :: (Integer, BasicDecimal)
      in x == fromIntegral n + f
    it "satisfies `properFraction` invariant 2" $
      property $ \x ->
      let (n,_) = properFraction (x :: BasicDecimal) :: (Integer, BasicDecimal)
      in (x < 0 && n <= 0) || (x >= 0 && n >= 0)
    it "satisfies `properFraction` invariant 3" $
      property $ \x ->
      let (_,f) = properFraction (x :: BasicDecimal) :: (Integer, BasicDecimal)
      in (x < 0 && f <= 0) || (x >= 0 && f >= 0)
    it "satisfies `properFraction` invariant 4" $
      property $ \x ->
      let (_,f) = properFraction (x :: BasicDecimal) :: (Integer, BasicDecimal)
      in isFinite f ==> abs f < 1

  describe "Floating" $ do
    it "produces same `pi` as Double" $
      realToFrac (pi :: ExtendedDecimal P16) `shouldBe` (pi :: Double)
    it "satisfies relation between (**) and (^)" $
      property $ \x y ->
      y >= 0 ==> (x :: BasicDecimal) ** fromInteger y == x ^ y
    it "computes `sqrt` correctly" $
      property $ \x ->
      isFinite x && x >= 0 ==>
      sqrt (x * x) `shouldBe` (x :: ExtendedDecimal P32)
      -- coefficient (sqrt (x * x) - (x :: ExtendedDecimal P16)) <= 1

  describe "RealFloat" $ do
    it "satisfies `decodeFloat` invariant 1" $
      property $ \x ->
      isFinite x ==> let b      = floatRadix (x :: BasicDecimal)
                         (m, n) = decodeFloat x
                     in x == fromInteger m * fromInteger b ^^ n
    it "satisfies `decodeFloat` invariant 2 (zero)" $
      decodeFloat (0 :: BasicDecimal) `shouldBe` (0,0)
    it "satisfies `decodeFloat` invariant 2 (negative zero)" $
      decodeFloat (read "-0" :: BasicDecimal) `shouldBe` (0,0)
    it "satisfies `decodeFloat` invariant 2 (nonzero)" $
      property $ \x ->
      isFinite x && x /= 0 ==> let b      = floatRadix (x :: BasicDecimal)
                                   (m, _) = decodeFloat x
                                   d      = floatDigits x
                                   am     = abs m
                               in b^(d-1) <= am && am < b^d
    it "satisfies relation between `encodeFloat` and `decodeFloat`" $
      property $ \x ->
      not (isNegativeZero x) ==>
      uncurry encodeFloat (decodeFloat x) == (x :: BasicDecimal)
    it "satisfies `exponent` invariant 1 (zero)" $
      exponent (0 :: BasicDecimal) `shouldBe` 0
    it "satisfies `exponent` invariant 1 (nonzero)" $
      property $ \x ->
      isFinite x && x /= 0 ==>
      exponent (x :: BasicDecimal) == snd (decodeFloat x) + floatDigits x
    it "satisfies `exponent` invariant 2" $
      property $ \x ->
      isFinite x ==> let b = floatRadix (x :: BasicDecimal)
                     in x == significand x * fromInteger b ^^ exponent x
    it "satisfies `significand` invariant" $
      property $ \x ->
      isFinite x ==> let s = significand (x :: BasicDecimal)
                         b = floatRadix x
                     in s == 0 ||
                        (s > -1 && s < 1 && abs s >= 1 / fromInteger b)

    it "detects negative zero" $
      isNegativeZero (read "-0" :: BasicDecimal) `shouldBe` True
    it "does not detect normal zero as negative" $
      isNegativeZero (read "+0" :: BasicDecimal) `shouldBe` False
    it "does not detect nonzero numbers as negative zero" $
      property $ \x ->
      x /= 0 ==> isNegativeZero (x :: BasicDecimal) == False

isFinite :: (FinitePrecision p, Rounding r) => Decimal p r -> Bool
isFinite x = not (isNaN x || isInfinite x)
