
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Decimal.Encoding (
    Decimal32, toDecimal32, fromDecimal32
  , Decimal64
  , Decimal128
  ) where

import Prelude hiding (exponent)

import Data.Binary (Binary(get, put), Put)
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, getWord16be, runBitGet)
import Data.Bits (Bits(bit, shiftL, shiftR, testBit, zeroBits), FiniteBits,
                  (.&.), (.|.))
import Data.Word (Word8, Word16)

import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

-- | A decimal floating point number with 7 digits of precision, rounding half
-- even, having a 32-bit encoded representation
newtype Decimal32 = Decimal32 (ExtendedDecimal P7)
                  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                            RealFloat, RealFrac, Bits, FiniteBits, Precision)

toDecimal32 :: Decimal p r -> Decimal32
toDecimal32 = Decimal32 . cast

fromDecimal32 :: (Precision p, Rounding r) => Decimal32 -> Decimal p r
fromDecimal32 (Decimal32 x) = cast x

instance Show Decimal32 where
  showsPrec d (Decimal32 n) = showsPrec d n

instance Read Decimal32 where
  readsPrec d str = [ (Decimal32 n, s) | (n, s) <- readsPrec d str ]

instance Binary Decimal32 where
  put = putDecimal32
  get = runBitGet getDecimal32

-- | A decimal floating point number with 16 digits of precision, rounding
-- half even, having a 64-bit encoded representation
newtype Decimal64 = Decimal64 (ExtendedDecimal P16)
                  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                            RealFloat, RealFrac, Bits, FiniteBits, Precision)

instance Show Decimal64 where
  showsPrec d (Decimal64 n) = showsPrec d n

instance Read Decimal64 where
  readsPrec d str = [ (Decimal64 n, s) | (n, s) <- readsPrec d str ]

instance Binary Decimal64 where
  put = putDecimal64
  get = runBitGet getDecimal64

-- | A decimal floating point number with 34 digits of precision, rounding
-- half even, having a 128-bit encoded representation
newtype Decimal128 = Decimal128 (ExtendedDecimal P34)
                   deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                             RealFloat, RealFrac, Bits, FiniteBits, Precision)

instance Show Decimal128 where
  showsPrec d (Decimal128 n) = showsPrec d n

instance Read Decimal128 where
  readsPrec d str = [ (Decimal128 n, s) | (n, s) <- readsPrec d str ]

instance Binary Decimal128 where
  put = putDecimal128
  get = runBitGet getDecimal128

-- Encoding and decoding functions

putDecimal32 :: Decimal32 -> Put
putDecimal32 = undefined

getDecimal32 :: BitGet Decimal32
getDecimal32 = Decimal32 <$> getDecimal 6 2 101

putDecimal64 :: Decimal64 -> Put
putDecimal64 = undefined

getDecimal64 :: BitGet Decimal64
getDecimal64 = Decimal64 <$> getDecimal 8 5 398

putDecimal128 :: Decimal128 -> Put
putDecimal128 = undefined

getDecimal128 :: BitGet Decimal128
getDecimal128 = Decimal128 <$> getDecimal 12 11 6176

-- Densely Packed Decimal

type BCD = (Bool, Bool, Bool, Bool)

type DPD = (Bool, Bool, Bool,
            Bool, Bool, Bool, Bool,
            Bool, Bool, Bool)

bcd2dpd :: BCD -> BCD -> BCD -> DPD
bcd2dpd (a, b, c, d) (e, f, g, h) (i, j, k, m) =
  let p = b || (a && j) || (a && f && i)
      q = c || (a && k) || (a && g && i)
      r = d
      s = (f && (not a || not i)) || (not a && e && j) || (e && i)
      t = g || (not a && e && k) || (a && i)
      u = h
      v = a || e || i
      w = a || (e && i) || (not e && j)
      x = e || (a && i) || (not a && k)
      y = m
  in (p, q, r, s, t, u, v, w, x, y)

dpd2bcd :: Word16 -> (Word8, Word8, Word8)
dpd2bcd dpd = (pack a b c d, pack e f g h, pack i j k m)

  where p = testBit dpd 9
        q = testBit dpd 8
        r = testBit dpd 7
        s = testBit dpd 6
        t = testBit dpd 5
        u = testBit dpd 4
        v = testBit dpd 3
        w = testBit dpd 2
        x = testBit dpd 1
        y = testBit dpd 0

        a = (v && w) && (not s || t || not x)
        b = p && (not v || not w || (s && not t && x))
        c = q && (not v || not w || (s && not t && x))
        d = r
        e = v && ((not w && x) || (not t && x) || (s && x))
        f = (s && (not v || not x)) || (p && not s && t && v && w && x)
        g = (t && (not v || not x)) || (q && not s && t && w)
        h = u
        i = v && ((not w && not x) || (w && x && (s || t)))
        j = (not v && w) || (s && v && not w && x) ||
            (p && w && (not x || (not s && not t)))
        k = (not v && x) || (t && not w && x) ||
            (q && v && w && (not x || (not s && not t)))
        m = y

        pack :: Bool -> Bool -> Bool -> Bool -> Word8
        pack a b c d = (if a then bit 3 else zeroBits) .|.
                       (if b then bit 2 else zeroBits) .|.
                       (if c then bit 1 else zeroBits) .|.
                       (if d then bit 0 else zeroBits)

digit2bcd :: Word8 -> BCD
digit2bcd d = case d of
  0 -> (False, False, False, False)
  1 -> (False, False, False, True )
  2 -> (False, False, True , False)
  3 -> (False, False, True , True )
  4 -> (False, True , False, False)
  5 -> (False, True , False, True )
  6 -> (False, True , True , False)
  7 -> (False, True , True , True )
  8 -> (True , False, False, False)
  9 -> (True , False, False, True )
  _ -> error "digit2bcd: invalid digit"

data CombinationField = Finite { exponentMSBs   :: Word8
                               , coefficientMSD :: Word8 }
                      | Infinity
                      | NotANumber

getCommon :: BitGet (Sign, CombinationField)
getCommon = do
  sign <- toEnum . fromEnum <$> getBool
  bits <- getWord8 5
  let cf = case bits of
        0x1e -> Infinity
        0x1f -> NotANumber
        _    -> let ab = shiftR bits 3 in case ab of
          0x03 -> Finite { exponentMSBs   = shiftR bits 1 .&. 0x03
                         , coefficientMSD = 0x08 .|. (bits .&. 0x01)
                         }
          _    -> Finite { exponentMSBs   = ab
                         , coefficientMSD = bits .&. 0x07
                         }
  return (sign, cf)

getCoefficient :: CombinationField -> Int -> BitGet Coefficient
getCoefficient cf = getCoefficient' (getMSD cf)

  where getCoefficient' :: Coefficient -> Int -> BitGet Coefficient
        getCoefficient' ic 0 = return ic
        getCoefficient' ic n = do
          (a, b, c) <- dpd2bcd <$> getWord16be 10
          let v = fromIntegral a * 100 + fromIntegral b * 10 + fromIntegral c
          getCoefficient' (ic * 1000 + v) (pred n)

        getMSD :: CombinationField -> Coefficient
        getMSD Finite { coefficientMSD = msd } = fromIntegral msd
        getMSD _                               = 0

getDecimal :: Int -> Int -> Exponent -> BitGet (Decimal p r)
getDecimal ecbits cclen bias = do
  (sign, cf) <- getCommon
  ec <- getWord16be ecbits
  coefficient <- getCoefficient cf cclen
  return $ case cf of
    Finite { exponentMSBs = msbs } ->
      let ee = shiftL (fromIntegral msbs) ecbits .|. fromIntegral ec
      in Num { sign = sign, coefficient = coefficient, exponent = ee - bias }
    Infinity -> Inf { sign = sign }
    NotANumber ->
      let s = testBit ec (ecbits - 1)
      in NaN { sign = sign, signaling = s, payload = coefficient }
