
{-# LANGUAGE FlexibleInstances #-}

-- | This module implements the decimal interchange format encodings described
-- in IEEE 754-2008, including the /decimal32/, /decimal64/, and /decimal128/
-- formats, as well as arbitrary width /decimal{k}/ formats through the use of
-- 'Format' with 'KPlus32' and\/or 'KTimes2'. For example, to use a
-- /decimal96/ format:
--
-- > type Decimal96 = ExtendedDecimal (Format (KPlus32 K64) DecimalCoefficient)
--
-- Currently only a decimal encoding of coefficients is implemented, but a
-- binary encoding may be added in the future.
module Numeric.Decimal.Encoding (
    -- * Primary convenience types
    Decimal32
  , Decimal64
  , Decimal128

    -- ** Precision types
  , Pdecimal32
  , Pdecimal64
  , Pdecimal128

    -- * Interchange format types
  , Format

    -- ** Format parameters
  , Parameters
  , K32
  , K64
  , K128
  , KPlus32
  , KTimes2

    -- ** Coefficient encodings
  -- , CoefficientEncoding
  , DecimalCoefficient
  , BinaryCoefficient
  ) where

import Prelude hiding (exponent)

import Data.Binary (Binary(get, put), Get)
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, getWord16be, runBitGet)
import Data.Binary.Bits.Put (BitPut, putBool, putWord8, putWord16be, runBitPut)
import Data.Bits (bit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Word (Word8, Word16)

import Numeric.Decimal.Number
import Numeric.Decimal.Precision

-- Decimal number types

-- | A decimal floating point number with 7 digits of precision, rounding half
-- even, and a 32-bit encoded representation using the /decimal32/ interchange
-- format (with a decimal encoding for the coefficient)
type Decimal32 = ExtendedDecimal Pdecimal32

-- | A decimal floating point number with 16 digits of precision, rounding
-- half even, and a 64-bit encoded representation using the /decimal64/
-- interchange format (with a decimal encoding for the coefficient)
type Decimal64 = ExtendedDecimal Pdecimal64

-- | A decimal floating point number with 34 digits of precision, rounding
-- half even, and a 128-bit encoded representation using the /decimal128/
-- interchange format (with a decimal encoding for the coefficient)
type Decimal128 = ExtendedDecimal Pdecimal128

-- Precision types

-- | A type with 'Precision' instance specifying /decimal32/ interchange
-- format parameters (using a decimal encoding for the coefficient) having an
-- effective precision of 7 decimal digits
type Pdecimal32 = Format K32 DecimalCoefficient

-- | A type with 'Precision' instance specifying /decimal64/ interchange
-- format parameters (using a decimal encoding for the coefficient) having an
-- effective precision of 16 decimal digits
type Pdecimal64 = Format K64 DecimalCoefficient

-- | A type with 'Precision' instance specifying /decimal128/ interchange
-- format parameters (using a decimal encoding for the coefficient) having an
-- effective precision of 34 decimal digits
type Pdecimal128 = Format K128 DecimalCoefficient

-- Format parameters

-- | Interchange format parameters used to define an encoding and derive the
-- format's /precision/ and E/max/
class Parameters k where
  -- | /k//32, the primary format parameter defining the encoding width as a
  -- multiple of 32 bits
  paramK32 :: k -> Int

-- | /p/, precision in digits
paramP :: Parameters k => k -> Int
paramP k = 9 * paramK32 k - 2

-- | /emax/
paramEmax :: Parameters k => k -> Exponent
paramEmax k = 3 * 2^(paramK32 k * 2 + 3)

-- | /bias/, /E/ − /q/
paramBias :: Parameters k => k -> Exponent
paramBias k = paramEmax k + fromIntegral (paramP k - 2)

-- | /w/, combination field width in bits − 5
paramW :: Parameters k => k -> Int
paramW k = paramK32 k * 2 + 4

-- | /t//10, trailing significand field width in 10-bit multiples
paramT10 :: Parameters k => k -> Int
paramT10 k = 3 * paramK32 k - 1

-- | Parameters for the /decimal32/ interchange format
data K32
instance Parameters K32 where
  paramK32 _ = 1

-- | Parameters for the /decimal64/ interchange format
type K64 = KPlus32 K32

-- | Parameters for the /decimal128/ interchange format
type K128 = KTimes2 K64

-- | Parameters for a /decimal{@k@ + 32}/ interchange format
data KPlus32 k
instance Parameters k => Parameters (KPlus32 k) where
  paramK32 t = paramK32 (minus32 t) + 1
    where minus32 :: KPlus32 k -> k
          minus32 = undefined

-- | Parameters for a /decimal{@k@ × 2}/ interchange format
data KTimes2 k
instance Parameters k => Parameters (KTimes2 k) where
  paramK32 t = paramK32 (div2 t) * 2
    where div2 :: KTimes2 k -> k
          div2 = undefined

-- | A class encapsulating coefficient encodings
class CoefficientEncoding c

-- | Specify a decimal encoding for the coefficient.
data DecimalCoefficient
instance CoefficientEncoding DecimalCoefficient

-- | Specify a binary encoding for the coefficient (currently unimplemented).
data BinaryCoefficient
instance CoefficientEncoding BinaryCoefficient

-- | A type (with a 'Precision' instance) for specifying interchange format
-- parameters @k@ and coefficient encoding @c@
data Format k c

formatK :: Format k c -> k
formatK = undefined

-- | This 'Precision' instance automatically computes the /precision/ and
-- E/max/ of decimal numbers that use this format.
instance Parameters k => Precision (Format k c) where
  precision = Just . paramP    . formatK
  eMax      = Just . paramEmax . formatK

instance Parameters k => FinitePrecision (Format k c)

-- | A 'Binary' instance is defined for interchange formats for which a
-- 'Parameters' instance exists, and covers particularly the 'Decimal32',
-- 'Decimal64', and 'Decimal128' types.
instance Parameters k => Binary (Decimal (Format k DecimalCoefficient) r) where
  put d = runBitPut $ putDecimal (paramW k) (paramT10 k) (paramBias k) d
    where k = formatK (decimalFormat d)

          decimalFormat :: Decimal (Format k c) r -> Format k c
          decimalFormat = undefined

  get = result
    where result = runBitGet $ getDecimal (paramW k) (paramT10 k) (paramBias k)
          k = formatK (getDecimalFormat result)

          getDecimalFormat :: Get (Decimal (Format k c) r) -> Format k c
          getDecimalFormat = undefined

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

-- Low-level encoding/decoding

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
