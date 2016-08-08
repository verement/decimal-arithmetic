
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Decimal.Encoding (
    Decimal32 , toDecimal32 , fromDecimal32
  , Decimal64 , toDecimal64 , fromDecimal64
  , Decimal128, toDecimal128, fromDecimal128
  ) where

import Prelude hiding (exponent)

import Data.Binary (Binary(get, put))
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, getWord16be, runBitGet)
import Data.Binary.Bits.Put (BitPut, putBool, putWord8, putWord16be, runBitPut)
import Data.Bits (Bits(bit, shiftL, shiftR, testBit, zeroBits), FiniteBits,
                  (.&.), (.|.))
import Data.Word (Word8, Word16)

import Numeric.Decimal.Number
import Numeric.Decimal.Precision

-- | A decimal floating point number with 7 digits of precision, rounding half
-- even, having a 32-bit encoded representation
newtype Decimal32 = Decimal32 { fromDecimal32 :: ExtendedDecimal P7 }
                  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                            RealFloat, RealFrac, Bits, FiniteBits, Precision)

toDecimal32 :: ExtendedDecimal P7 -> Decimal32
toDecimal32 = Decimal32

instance Show Decimal32 where
  showsPrec d (Decimal32 n) = showsPrec d n

instance Read Decimal32 where
  readsPrec d str = [ (Decimal32 n, s) | (n, s) <- readsPrec d str ]

-- | The 'Binary' instance implements the /decimal32/ interchange format
-- described in IEEE 754-2008 using a /decimal/ encoding for the coefficient.
instance Binary Decimal32 where
  put = runBitPut . putDecimal32
  get = runBitGet   getDecimal32

-- | A decimal floating point number with 16 digits of precision, rounding
-- half even, having a 64-bit encoded representation
newtype Decimal64 = Decimal64 { fromDecimal64 :: ExtendedDecimal P16 }
                  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                            RealFloat, RealFrac, Bits, FiniteBits, Precision)

toDecimal64 :: ExtendedDecimal P16 -> Decimal64
toDecimal64 = Decimal64

instance Show Decimal64 where
  showsPrec d (Decimal64 n) = showsPrec d n

instance Read Decimal64 where
  readsPrec d str = [ (Decimal64 n, s) | (n, s) <- readsPrec d str ]

-- | The 'Binary' instance implements the /decimal64/ interchange format
-- described in IEEE 754-2008 using a /decimal/ encoding for the coefficient.
instance Binary Decimal64 where
  put = runBitPut . putDecimal64
  get = runBitGet   getDecimal64

-- | A decimal floating point number with 34 digits of precision, rounding
-- half even, having a 128-bit encoded representation
newtype Decimal128 = Decimal128 { fromDecimal128 :: ExtendedDecimal P34 }
                   deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real,
                             RealFloat, RealFrac, Bits, FiniteBits, Precision)

toDecimal128 :: ExtendedDecimal P34 -> Decimal128
toDecimal128 = Decimal128

instance Show Decimal128 where
  showsPrec d (Decimal128 n) = showsPrec d n

instance Read Decimal128 where
  readsPrec d str = [ (Decimal128 n, s) | (n, s) <- readsPrec d str ]

-- | The 'Binary' instance implements the /decimal128/ interchange format
-- described in IEEE 754-2008 using a /decimal/ encoding for the coefficient.
instance Binary Decimal128 where
  put = runBitPut . putDecimal128
  get = runBitGet   getDecimal128

-- Encoding and decoding functions

putDecimal32 :: Decimal32 -> BitPut ()
putDecimal32 = putDecimal 6 2 101 . fromDecimal32

getDecimal32 :: BitGet Decimal32
getDecimal32 = toDecimal32 <$> getDecimal 6 2 101

putDecimal64 :: Decimal64 -> BitPut ()
putDecimal64 = putDecimal 8 5 398 . fromDecimal64

getDecimal64 :: BitGet Decimal64
getDecimal64 = toDecimal64 <$> getDecimal 8 5 398

putDecimal128 :: Decimal128 -> BitPut ()
putDecimal128 = putDecimal 12 11 6176 . fromDecimal128

getDecimal128 :: BitGet Decimal128
getDecimal128 = toDecimal128 <$> getDecimal 12 11 6176

-- Densely Packed Decimal

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

bcd2dpd :: Word8 -> Word8 -> Word8 -> Word16
bcd2dpd d1 d2 d3 =
  (if p then bit 9 else zeroBits) .|.
  (if q then bit 8 else zeroBits) .|.
  (if r then bit 7 else zeroBits) .|.
  (if s then bit 6 else zeroBits) .|.
  (if t then bit 5 else zeroBits) .|.
  (if u then bit 4 else zeroBits) .|.
  (if v then bit 3 else zeroBits) .|.
  (if w then bit 2 else zeroBits) .|.
  (if x then bit 1 else zeroBits) .|.
  (if y then bit 0 else zeroBits)

  where a = testBit d1 3
        b = testBit d1 2
        c = testBit d1 1
        d = testBit d1 0

        e = testBit d2 3
        f = testBit d2 2
        g = testBit d2 1
        h = testBit d2 0

        i = testBit d3 3
        j = testBit d3 2
        k = testBit d3 1
        m = testBit d3 0

        p = b || (a && j) || (a && f && i)
        q = c || (a && k) || (a && g && i)
        r = d
        s = (f && (not a || not i)) || (not a && e && j) || (e && i)
        t = g || (not a && e && k) || (a && i)
        u = h
        v = a || e || i
        w = a || (e && i) || (not e && j)
        x = e || (a && i) || (not a && k)
        y = m

-- Low-level functions

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

putCommon :: Sign -> CombinationField -> BitPut ()
putCommon sign cf = do
  putBool (toEnum . fromEnum $ sign)
  let bits = case cf of
        Finite { exponentMSBs = msbs, coefficientMSD = msd }
          | msd < 8   ->          shiftL msbs 3 .|.  msd
          | otherwise -> 0x18 .|. shiftL msbs 1 .|. (msd .&. 0x01)
        Infinity      -> 0x1e
        NotANumber    -> 0x1f
  putWord8 5 bits

getCoefficient :: CombinationField -> Int -> BitGet Coefficient
getCoefficient = getCoefficient' . getMSD

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

putDecimal :: Int -> Int -> Exponent -> Decimal p r -> BitPut ()
putDecimal ecbits cclen bias x = do
  let msd : cc = digits x
      (cf, ee) = case x of
        Num { exponent = e } ->
          let cf = Finite { exponentMSBs   = fromIntegral (shiftR ee ecbits)
                          , coefficientMSD = msd
                          }
          in (cf, fromIntegral $ e + bias)
        Inf{} -> (Infinity, 0)
        NaN { signaling = s } -> (NotANumber, if s then bit (ecbits - 1) else 0)
  putCommon (sign x) cf
  putWord16be ecbits (ee .&. (bit ecbits - 1))
  putDigits cc

  where digits :: Decimal p r -> [Word8]
        digits x = let ds = case x of
                         Num { coefficient = c } -> digits' c
                         NaN { payload     = p } -> digits' p
                         Inf {                 } -> []
                   in replicate (1 + cclen * 3 - length ds) 0 ++ ds

        digits' :: Coefficient -> [Word8]
        digits' = go []
          where go ds 0 = ds
                go ds c = let (q, r) = c `quotRem` 10
                          in go (fromIntegral r : ds) q

        putDigits :: [Word8] -> BitPut ()
        putDigits (a : b : c : rest) = do
          putWord16be 10 (bcd2dpd a b c)
          putDigits rest
        putDigits [] = return ()
        putDigits _ = error "putDigits: invalid # digits"
