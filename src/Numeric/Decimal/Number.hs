
module Numeric.Decimal.Number
       ( Sign(..)
       , negateSign
       , xorSigns

       , Coefficient
       , numDigits

       , Exponent
       , Payload

       , Number(..)
       , zero
       , one
       , negativeOne
       , infinity
       , qNaN
       , sNaN

       , flipSign
       , cast

       , isPositive
       , isNegative
       , isFinite
       , isZero
       , isNormal
       , isSubnormal
       ) where

import Prelude hiding (exponent, round)

import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)

import {-# SOURCE #-} Numeric.Decimal.Conversion
import {-# SOURCE #-} Numeric.Decimal.Arithmetic
import                Numeric.Decimal.Precision
import                Numeric.Decimal.Rounding

import {-# SOURCE #-} qualified Numeric.Decimal.Operation as Op

import qualified GHC.Real

{- $setup
>>> :load Harness
-}

data Sign = Pos | Neg
          deriving (Eq, Enum, Show)

negateSign :: Sign -> Sign
negateSign Pos = Neg
negateSign Neg = Pos

xorSigns :: Sign -> Sign -> Sign
xorSigns Pos Pos = Pos
xorSigns Pos Neg = Neg
xorSigns Neg Pos = Neg
xorSigns Neg Neg = Pos

signFunc :: Num a => Sign -> a -> a
signFunc Pos = id
signFunc Neg = negate

signMatch :: (Num a, Eq a) => a -> Sign
signMatch x = case signum x of
  -1 -> Neg
  _  -> Pos

type Coefficient = Natural
type Exponent    = Int
type Payload     = Coefficient

-- | A decimal floating point number with selectable precision and rounding
-- algorithm
data Number p r
  = Num  { sign        :: Sign
         , coefficient :: Coefficient
         , exponent    :: Exponent
         }
  | Inf  { sign        :: Sign
         }
  | QNaN { sign        :: Sign
         , payload     :: Payload
         }
  | SNaN { sign        :: Sign
         , payload     :: Payload
         }

instance Show (Number p r) where
  showsPrec d n = showParen (d > 0 && isNegative n) $ toScientificString n

instance (Precision p, Rounding r) => Read (Number p r) where
  readsPrec _ str = [ (cast n, s)
                    | (n, s) <- readParen False
                      (readP_to_S toNumber . dropWhile isSpace) str ]

{- $doctest-Read
>>> fmap toRep (read "Just 123" :: Maybe GeneralDecimal)
Just (N (0,123,0))

>>> fmap toRep (read "Just (-12.0)" :: Maybe GeneralDecimal)
Just (N (1,120,-1))
-}

instance Precision p => Precision (Number p r) where
  precision = precision . numberPrecision
    where numberPrecision :: Number p r -> p
          numberPrecision = undefined

numberOp :: (Precision p, Rounding r) => Arith p r (Number p r) -> Number p r
numberOp op = either exceptionResult id $ evalArith op newContext

type GeneralNumber = Number PInfinite RoundDown

instance Eq (Number p r) where
  x == y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { coefficient = 0 } -> True
    _                       -> False

instance Ord (Number p r) where
  x `compare` y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { coefficient = 0 } -> EQ
    Num { sign = Neg      } -> LT
    Num { sign = Pos      } -> GT
    _                       -> GT  -- match Prelude behavior for NaN

  x < y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { sign = Neg      } -> True
    _                       -> False

  x <= y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { sign = Neg      } -> True
    Num { coefficient = 0 } -> True
    _                       -> False

  x > y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { coefficient = 0 } -> False
    Num { sign = Pos      } -> True
    _                       -> False

  x >= y = case numberOp (x `Op.compare` y) :: GeneralNumber of
    Num { sign = Pos      } -> True
    _                       -> False

  max nan@SNaN{} _ = nan
  max _ nan@SNaN{} = nan
  max nan@QNaN{} _ = nan
  max _ nan@QNaN{} = nan
  max x y
    | x >= y    = x
    | otherwise = y

  min nan@SNaN{} _ = nan
  min _ nan@SNaN{} = nan
  min nan@QNaN{} _ = nan
  min _ nan@QNaN{} = nan
  min x y
    | x < y     = x
    | otherwise = y

instance (Precision p, Rounding r) => Enum (Number p r) where
  succ x = numberOp (x `Op.add`      one)
  pred x = numberOp (x `Op.subtract` one)

  toEnum = fromIntegral

  fromEnum Num { sign = s, coefficient = c, exponent = e }
    | e >= 0    = signFunc s (fromIntegral   c  *      10^  e  )
    | otherwise = signFunc s (fromIntegral $ c `quot` (10^(-e)))
  fromEnum _ = 0

  enumFrom       x     = enumFromWith x one
  enumFromThen   x y   = let i = y - x
                         in x : enumFromWith y i
  enumFromTo     x   e = takeWhile (<= e) $ enumFromWith x one
  enumFromThenTo x y e = let i = y - x
                             cmp | i < 0     = (>=)
                                 | otherwise = (<=)
                         in takeWhile (`cmp` e) $ x : enumFromWith y i

enumFromWith :: (Precision p, Rounding r)
             => Number p r -> Number p r -> [Number p r]
enumFromWith x i = x : enumFromWith (x + i) i

{- $doctest-Enum
>>> [0, 0.1 .. 2] :: [BasicDecimal]
[0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]

>>> [2, 1.9 .. 0] :: [BasicDecimal]
[2,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.2,1.1,1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.0]

>>> [1.7 .. 5.7] :: [BasicDecimal]
[1.7,2.7,3.7,4.7,5.7]
-}

instance (Precision p, Rounding r) => Num (Number p r) where
  x + y = numberOp (x `Op.add`      y)
  x - y = numberOp (x `Op.subtract` y)
  x * y = numberOp (x `Op.multiply` y)

  negate = numberOp . Op.minus
  abs    = numberOp . Op.abs

  signum n = case n of
    Num { coefficient = 0 } -> zero
    Num { sign = s        } -> one { sign = s }
    Inf { sign = s        } -> one { sign = s }
    _                       -> n

  fromInteger x = cast
    Num { sign        = signMatch x
        , coefficient = fromInteger (abs x)
        , exponent    = 0
        }

{- $doctest-Num
prop> x + x == x * (2 :: GeneralDecimal)
prop> isFinite x ==> x - x == (0 :: GeneralDecimal)
prop> isFinite x ==> x + negate x == (0 :: GeneralDecimal)
prop> abs x >= (0 :: GeneralDecimal)

prop> abs x * signum x == (x :: GeneralDecimal)
-}

instance (Precision p, Rounding r) => Real (Number p r) where
  toRational Num { sign = s, coefficient = c, exponent = e }
    | e >= 0    = fromInteger $ signFunc s (fromIntegral c * 10^e)
    | otherwise = signFunc s (fromIntegral c) % 10^(-e)
  toRational n = signFunc (sign n) $ case n of
    Inf{} -> GHC.Real.infinity
    _     -> GHC.Real.notANumber

instance (FinitePrecision p, Rounding r) => Fractional (Number p r) where
  x / y = numberOp (x `Op.divide` y)
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (FinitePrecision p, Rounding r) => RealFrac (Number p r) where
  properFraction x@Num { sign = s, coefficient = c, exponent = e }
    | e < 0     = (n, f)
    | otherwise = (signFunc s (fromIntegral c * 10^e), zero)
    where n = signFunc s (fromIntegral q)
          f = x { coefficient = r }
          (q, r) = c `quotRem` (10^(-e))
  properFraction nan = (0, nan)

{- $doctest-RealFrac
prop> let (n,f) = properFraction (x :: BasicDecimal) in x == fromIntegral n + f
prop> let (n,f) = properFraction (x :: BasicDecimal) in (x < 0 && n <= 0) || (x >= 0 && n >= 0)
prop> let (n,f) = properFraction (x :: BasicDecimal) in (x < 0 && f <= 0) || (x >= 0 && f >= 0)
prop> let (n,f) = properFraction (x :: BasicDecimal) in isFinite f ==> abs f < 1
-}

-- | Compute an infinite series to maximum precision.
infiniteSeries :: (FinitePrecision p, Rounding r) => [Number p r] -> Number p r
infiniteSeries = series zero
  where series n (x:xs)
          | n' == n   = n'
          | otherwise = series n' xs
          where n' = n + x
        series n []   = n

-- | Compute the arcsine of the argument to maximum precision using series
-- expansion.
arcsine :: (FinitePrecision p, Rounding r) => Number p r -> Number p r
arcsine x = infiniteSeries (x : series 1 2 x 3)
  where series n d x i =
          let x' = x * x2
          in (n * x') / (d * i) : series (n * i) (d * (i + one)) x' (i + two)
        x2 = x * x

-- | Compute π to maximum precision using the arcsine series expansion.
seriesPi :: FinitePrecision p => Number p RoundHalfEven
seriesPi = 6 * arcsine oneHalf

-- | Cast a number with two additional digits of precision down to a number
-- with the desired precision.
castDown :: (Precision p, Rounding r)
         => Number (PPlus1 (PPlus1 p)) a -> Number p r
castDown = cast

notyet :: String -> a
notyet s = error (s ++ ": not yet implemented")

instance (FinitePrecision p, Rounding r) => Floating (Number p r) where
  pi = castDown seriesPi

  exp   = notyet "exp"
  log   = notyet "log"

  sin   = notyet "sin"
  cos   = notyet "cos"
  asin  = notyet "asin"
  acos  = notyet "acos"
  atan  = notyet "atan"
  sinh  = notyet "sinh"
  cosh  = notyet "cosh"
  asinh = notyet "asinh"
  acosh = notyet "acosh"
  atanh = notyet "atanh"

{- $doctest-Floating
prop> realToFrac (pi :: ExtendedDecimal P16) == (pi :: Double)
-}

instance (FinitePrecision p, Rounding r) => RealFloat (Number p r) where
  floatRadix  _ = 10
  floatDigits x = let Just p = precision x in p
  floatRange  _ = (minBound, maxBound)  -- ?

  decodeFloat x = case x of
    Num  { sign = s, coefficient = c, exponent = e } -> (m, n)
      where m = signFunc s (fromIntegral c)
            n = fromIntegral e
    Inf  { sign = s              } -> (special s 0, maxBound    )
    QNaN { sign = s, payload = p } -> (special s p, minBound    )
    SNaN { sign = s, payload = p } -> (special s p, minBound + 1)
    where pp = 10 ^ floatDigits x :: Integer
          special :: Sign -> Coefficient -> Integer
          special s v = signFunc s (pp + fromIntegral v)

  encodeFloat m n = x
    where x | am >= pp  = special
            | otherwise = cast Num { sign        = signMatch m
                                   , coefficient = fromInteger am
                                   , exponent    = fromIntegral n
                                   }
          special
            | n == maxBound     = Inf  { sign = signMatch m }
            | n == minBound     = QNaN { sign = signMatch m, payload = p }
            | otherwise         = SNaN { sign = signMatch m, payload = p }
            where p = fromInteger (am - pp)
          am = abs m              :: Integer
          pp = 10 ^ floatDigits x :: Integer

  isNaN x = case x of
    QNaN{} -> True
    SNaN{} -> True
    _      -> False

  isInfinite x = case x of
    Inf{} -> True
    _     -> False

  isDenormalized = isSubnormal

  isNegativeZero x = case x of
    Num { sign = Neg, coefficient = 0 } -> True
    _                                   -> False

  isIEEE _ = True

{- $doctest-RealFloat
prop> uncurry encodeFloat (decodeFloat x) == (x :: BasicDecimal)
prop> isFinite x ==> significand x * fromInteger (floatRadix x) ^^ Prelude.exponent x == (x :: BasicDecimal)
prop> Prelude.exponent (0 :: BasicDecimal) == 0
prop> isFinite x && x /= 0 ==> Prelude.exponent (x :: BasicDecimal) == snd (decodeFloat x) + floatDigits x

prop> isNegativeZero (read "-0" :: BasicDecimal) == True
prop> isNegativeZero (read "+0" :: BasicDecimal) == False
prop> x /= 0 ==> isNegativeZero (x :: BasicDecimal) == False
-}

-- | A 'Number' representing the value zero
zero :: Number p r
zero = Num { sign        = Pos
           , coefficient = 0
           , exponent    = 0
           }

-- | A 'Number' representing the value ½
oneHalf :: Number p r
oneHalf = zero { coefficient = 5, exponent = -1 }

-- | A 'Number' representing the value one
one :: Number p r
one = zero { coefficient = 1 }

-- | A 'Number' representing the value two
two :: Number p r
two = zero { coefficient = 2 }

-- | A 'Number' representing the value negative one
negativeOne :: Number p r
negativeOne = one { sign = Neg }

-- | A 'Number' representing the value positive infinity
infinity :: Number p r
infinity = Inf { sign = Pos }

-- | A 'Number' representing undefined results
qNaN :: Number p r
qNaN = QNaN { sign = Pos, payload = 0 }

-- | A signaling 'Number' representing undefined results
sNaN :: Number p r
sNaN = SNaN { sign = Pos, payload = 0 }

-- | Negate the given 'Number' by directly flipping its sign.
flipSign :: Number p r -> Number p r
flipSign n = n { sign = negateSign (sign n) }

-- | Cast a 'Number' to another precision and/or rounding algorithm,
-- immediately rounding if necessary to the new precision using the new
-- algorithm.
cast :: (Precision p, Rounding r) => Number a b -> Number p r
cast = numberOp . round . coerce

-- | Return the number of decimal digits of the argument.
numDigits :: Coefficient -> Int
numDigits x
  | x <         10 = 1
  | x <        100 = 2
  | x <       1000 = 3
  | x <      10000 = 4
  | x <     100000 = 5
  | x <    1000000 = 6
  | x <   10000000 = 7
  | x <  100000000 = 8
  | x < 1000000000 = 9
  | otherwise      = 9 + numDigits (x `quot` 1000000000)

maxCoefficient :: Precision p => p -> Maybe Coefficient
maxCoefficient p = (\d -> 10 ^ d - 1) <$> precision p

-- | Is the sign of the given 'Number' positive?
isPositive :: Number p r -> Bool
isPositive n = case sign n of
  Pos -> True
  Neg -> False

-- | Is the sign of the given 'Number' negative?
isNegative :: Number p r -> Bool
isNegative n = case sign n of
  Neg -> True
  Pos -> False

-- | Does the given 'Number' represent a finite value?
isFinite :: Number p r -> Bool
isFinite Num{} = True
isFinite _     = False

-- | Does the given 'Number' represent the value zero?
isZero :: Number p r -> Bool
isZero Num { coefficient = 0 } = True
isZero _                       = False

-- | Is the given 'Number' normal?
isNormal :: Precision p => Number p r -> Bool
isNormal n
  | isFinite n && not (isZero n) = maybe True (adjustedExponent n >=) (eMin n)
  | otherwise                    = False

-- | Is the given 'Number' subnormal?
isSubnormal :: Precision p => Number p r -> Bool
isSubnormal n
  | isFinite n && not (isZero n) = maybe False (adjustedExponent n <) (eMin n)
  | otherwise                    = False

-- | Upper limit on the absolute value of the exponent
eLimit :: Precision p => p -> Maybe Exponent
eLimit = eMax -- ?

-- | Minimum value of the adjusted exponent
eMin :: Precision p => p -> Maybe Exponent
eMin n = (1 -) <$> eMax n

-- | Maximum value of the adjusted exponent
eMax :: Precision p => p -> Maybe Exponent
eMax n = subtract 1 . (10 ^) . numDigits <$> base
  where mlength = precision n                    :: Maybe Int
        base = (10 *) . fromIntegral <$> mlength :: Maybe Coefficient

-- | Minimum value of the exponent for subnormal results
eTiny :: Precision p => p -> Maybe Exponent
eTiny n = (-) <$> eMin n <*> (fromIntegral . subtract 1 <$> precision n)

-- | Range of permissible exponent values
eRange :: Precision p => Number p r -> Maybe (Exponent, Exponent)
eRange n@Num { coefficient = c } = range <$> eLimit n
  where range :: Exponent -> (Exponent, Exponent)
        range lim = (-lim - clm1 + 1, lim - clm1)
        clength = numDigits c             :: Int
        clm1 = fromIntegral (clength - 1) :: Exponent
eRange _ = Nothing

adjustedExponent :: Number p r -> Exponent
adjustedExponent Num { coefficient = c, exponent = e } =
  e + fromIntegral (clength - 1)
  where clength = numDigits c :: Int
adjustedExponent _ = error "adjustedExponent: not a finite number"
