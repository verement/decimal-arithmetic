
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
import Data.Bits (Bits(bit, shiftL, shiftR, testBit), FiniteBits, (.&.), (.|.))
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
dpd2bcd dpd = case mask 0 0xe of
  0xe -> case mask 4 0x6 of
    0x6 -> (    c7,     f4,     i0)
    0x4 -> (a9b8c7,     f4,     i0)
    0x2 -> (    c7, d9e8f4,     i0)
    _   -> (    c7,     f4, g9h8i0)
  0xc ->   (    c7, d6e5f4, g9h8i0)
  0xa ->   (a9b8c7,     f4, g6h5i0)
  0x8 ->   (a9b8c7, d6e5f4,     i0)
  _   ->   (a9b8c7, d6e5f4, g2h1i0)

  where a9b8c7 =              mask 7 7
        d6e5f4 =              mask 4 7
        d9e8f4 = mask 7 6 .|. mask 4 1
        g2h1i0 =              mask 0 7
        g6h5i0 = mask 4 6 .|. mask 0 1
        g9h8i0 = mask 7 6 .|. mask 0 1
        i0     = 8        .|. mask 0 1
        f4     = 8        .|. mask 4 1
        c7     = 8        .|. mask 7 1

        mask :: Int -> Word8 -> Word8
        mask s m = fromIntegral (shiftR dpd s) .&. m

bcd2dpd :: Word8 -> Word8 -> Word8 -> Word16
bcd2dpd d2 d1 d0 = case (d2 < 8, d1 < 8, d0 < 8) of
  (True , True , True ) ->      a9b8c7 .|.      d6e5f4          .|. g2h1i0
  (True , True , False) ->      a9b8c7 .|.      d6e5f4 .|. 0x08 .|.     i0
  (True , False, True ) ->      a9b8c7 .|. g6h5 .|. f4 .|. 0x0a .|.     i0
  (False, True , True ) -> g9h8 .|. c7 .|.      d6e5f4 .|. 0x0c .|.     i0
  (False, False, True ) -> g9h8 .|. c7 .|.          f4 .|. 0x0e .|.     i0
  (False, True , False) -> d9e8 .|. c7 .|.          f4 .|. 0x2e .|.     i0
  (True , False, False) ->      a9b8c7 .|.          f4 .|. 0x4e .|.     i0
  (False, False, False) ->          c7 .|.          f4 .|. 0x6e .|.     i0

  where a9b8c7 = isolate d2 7 7
        c7     = isolate d2 1 7
        d6e5f4 = isolate d1 7 4
        d9e8   = isolate d1 6 7
        f4     = isolate d1 1 4
        g2h1i0 = isolate d0 7 0
        g6h5   = isolate d0 6 4
        g9h8   = isolate d0 6 7
        i0     = isolate d0 1 0

        isolate :: Word8 -> Word8 -> Int -> Word16
        isolate d m = shiftL (fromIntegral $ d .&. m)

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
