
module Numeric.Decimal.Number
       ( Sign(..)
       , negateSign
       , xorSigns

       , Coefficient
       , numDigits

       , Exponent
       , Payload

       , Decimal(..)
       , BasicDecimal
       , ExtendedDecimal
       , GeneralDecimal

       , zero
       , oneHalf
       , one
       , negativeOne
       , two
       , ten
       , infinity
       , qNaN
       , sNaN

       , flipSign
       , cast

       , fromBool
       , fromOrdering

       , isPositive
       , isNegative
       , isFinite
       , isZero
       , isNormal
       , isSubnormal

       , adjustedExponent
       , integralValue
       ) where

import Prelude hiding (exponent)

import Control.DeepSeq (NFData(..))
import Control.Monad (join)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)

import {-# SOURCE #-} Numeric.Decimal.Arithmetic
import {-# SOURCE #-} Numeric.Decimal.Conversion
import                Numeric.Decimal.Precision
import                Numeric.Decimal.Rounding

import {-# SOURCE #-} qualified Numeric.Decimal.Operation as Op

import qualified GHC.Real

data Sign = Pos  -- ^ Positive or non-negative
          | Neg  -- ^ Negative
          deriving (Eq, Enum)

instance NFData Sign where
  rnf s = s `seq` ()

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
type Exponent    = Integer
type Payload     = Coefficient

-- | A decimal floating point number with selectable precision and rounding
-- algorithm
data Decimal p r
  = Num { sign        :: Sign
        , coefficient :: Coefficient
        , exponent    :: Exponent
        }
  | Inf { sign        :: Sign
        }
  | NaN { sign        :: Sign
        , signaling   :: Bool
        , payload     :: Payload
        }

-- | A decimal floating point number with 9 digits of precision, rounding half
-- up
type BasicDecimal = Decimal P9 RoundHalfUp

-- | A decimal floating point number with selectable precision, rounding half
-- even
type ExtendedDecimal p = Decimal p RoundHalfEven

-- | A decimal floating point number with infinite precision
type GeneralDecimal = ExtendedDecimal PInfinite

-- | The 'Show' instance uses the 'toScientificString' operation from
-- "Numeric.Decimal.Conversion".
instance Show (Decimal p r) where
  showsPrec d n = showParen (d > 0 && isNegative n) $ toScientificString n

-- | The 'Read' instance uses the 'toNumber' operation from
-- "Numeric.Decimal.Conversion" and rounds the result to the required
-- precision.
instance (Precision p, Rounding r) => Read (Decimal p r) where
  readsPrec _ str = [ (cast n, s)
                    | (n, s) <- readParen False
                      (readP_to_S toNumber . dropWhile isSpace) str ]

decimalPrecision :: Decimal p r -> p
decimalPrecision = undefined

instance Precision p => Precision (Decimal p r) where
  precision = precision . decimalPrecision
  eMax      = eMax      . decimalPrecision
  eMin      = eMin      . decimalPrecision

-- This assumes the arithmetic operation does not trap any signals, which
-- could result in an exception being thrown (and returned in a Left value).
evalOp :: Arith p r a -> a
evalOp op = let Right r = evalArith op newContext in r

evalOp' :: Arith p RoundHalfEven a -> a
evalOp' = evalOp

compareDecimal :: Decimal a b -> Decimal c d -> Either GeneralDecimal Ordering
compareDecimal x y = evalOp (x `Op.compare` y)

-- | Note that NaN values are not equal to any value, including other NaNs.
instance Eq (Decimal p r) where
  x == y = case compareDecimal x y of
    Right EQ -> True
    _        -> False

-- | Unlike the instances for 'Float' and 'Double', the 'compare' method in
-- this instance uses a total ordering over all possible values. Note that
-- @'compare' x y == 'EQ'@ does not imply @x == y@ (and similarly for 'LT' and
-- 'GT') in the cases where @x@ or @y@ are NaN values.
instance Ord (Decimal p r) where
  compare x y = case compareDecimal x y of
    Right o -> o
    Left _  -> evalOp (x `Op.compareTotal` y)

  x < y = case compareDecimal x y of
    Right LT -> True
    _        -> False

  x <= y = case compareDecimal x y of
    Right LT -> True
    Right EQ -> True
    _        -> False

  x > y = case compareDecimal x y of
    Right GT -> True
    _        -> False

  x >= y = case compareDecimal x y of
    Right GT -> True
    Right EQ -> True
    _        -> False

  max x y = evalOp (Op.max x y)
  min x y = evalOp (Op.min x y)

-- | Unlike the instances for 'Float' and 'Double', the lists returned by the
-- 'enumFromTo' and 'enumFromThenTo' methods in this instance terminate with
-- the last element strictly less than (greater than in the case of a negative
-- increment) or equal to the given bound.
instance (Precision p, Rounding r) => Enum (Decimal p r) where
  succ x = evalOp (x `Op.add`      one)
  pred x = evalOp (x `Op.subtract` one)

  toEnum = fromIntegral

  fromEnum Num { sign = s, coefficient = c, exponent = e }
    | e >= 0    = signFunc s (fromIntegral   c  *      10^  e  )
    | otherwise = signFunc s (fromIntegral $ c `quot` (10^(-e)))
  fromEnum _ = 0

  enumFrom       x     = enumFromWith x one
  enumFromThen   x y   = let i = y - x
                         in x : enumFromWith y i
  enumFromTo     x   z = takeWhile (<= z) $ enumFromWith x one
  enumFromThenTo x y z = let i = y - x
                             cmp | i < 0     = (>=)
                                 | otherwise = (<=)
                         in takeWhile (`cmp` z) $ x : enumFromWith y i

enumFromWith :: (Precision p, Rounding r)
             => Decimal p r -> Decimal p r -> [Decimal p r]
enumFromWith x i = x : enumFromWith (x + i) i

instance (Precision p, Rounding r) => Num (Decimal p r) where
  x + y = evalOp (x `Op.add`      y)
  x - y = evalOp (x `Op.subtract` y)
  x * y = evalOp (x `Op.multiply` y)

  negate = evalOp . Op.minus
  abs    = evalOp . Op.abs

  signum n = case n of
    Num { sign = s, coefficient = c }
      | c == 0       -> zero { sign = s }
      | otherwise    -> one  { sign = s }
    Inf { sign = s } -> one  { sign = s }
    _                -> qNaN

  fromInteger x = cast
    Num { sign        = signMatch x
        , coefficient = fromInteger (abs x)
        , exponent    = 0
        }

instance (Precision p, Rounding r) => Real (Decimal p r) where
  toRational Num { sign = s, coefficient = c, exponent = e }
    | e >= 0    = fromInteger $ signFunc s (fromIntegral c * 10^e)
    | otherwise = signFunc s (fromIntegral c) % 10^(-e)
  toRational n = signFunc (sign n) $ case n of
    Inf{} -> GHC.Real.infinity
    _     -> GHC.Real.notANumber

instance (FinitePrecision p, Rounding r) => Fractional (Decimal p r) where
  x / y = evalOp (x `Op.divide` y)
  fromRational r = let n = fromInteger (numerator   r) :: GeneralDecimal
                       d = fromInteger (denominator r) :: GeneralDecimal
                   in evalOp (n `Op.divide` d)

instance (FinitePrecision p, Rounding r) => RealFrac (Decimal p r) where
  properFraction x@Num { sign = s, coefficient = c, exponent = e }
    | e < 0     = (n, f)
    | otherwise = (signFunc s (fromIntegral c * 10^e), zero)
    where n = signFunc s (fromIntegral q)
          f = x { coefficient = r }
          (q, r) = c `quotRem` (10^(-e))
  properFraction nan = (0, nan)

-- | Compute a generalized continued fraction to maximum precision. A hint is
-- used to indicate the minimum number of terms that should be generated
-- before (expensively) examining the results for convergence.
continuedFraction :: FinitePrecision p
                  => Int -> Integer -> [(Integer, Integer)] -> ExtendedDecimal p
continuedFraction m b0 ((a1, b1) : ps) = convergent (max 0 $ m - 2) x0 x1 x1' ps
  where x0  = (aa0, bb0)
        x1  = (aa1, bb1)
        x1' = fromRational (aa1 % bb1)
        aa0 = b0
        bb0 = 1
        aa1 = b1 * b0 + a1
        bb1 = b1

        convergent m (aa0, bb0) x1@(aa1, bb1) x1' ((a2, b2) : ps)
          | m == 0 && x2' == x1' = x2'
          | otherwise            = convergent (max 0 $ m - 1) x1 x2 x2' ps
          where x2  = (aa2, bb2)
                x2' = fromRational (aa2 % bb2)
                aa2 = b2 * aa1 + a2 * aa0
                bb2 = b2 * bb1 + a2 * bb0
        convergent _ _ _ x [] = x

continuedFraction _ b0 [] = fromInteger b0

-- | Compute an infinite series to maximum precision.
infiniteSeries :: FinitePrecision p
               => (ExtendedDecimal p -> ExtendedDecimal p -> ExtendedDecimal p)
               -> [ExtendedDecimal p] -> ExtendedDecimal p
infiniteSeries op ~(x:xs) = series x xs
  where series n (x:xs)
          | n' == n   = n'
          | otherwise = series n' xs
          where n' = n `op` x
        series n []   = n

-- | Compute the inverse tangent of the argument to maximum precision using
-- series expansion.
seriesArctan :: FinitePrecision p => Decimal p r -> ExtendedDecimal p
seriesArctan z = infiniteSeries (+) (z' : series True three z')
  where series neg d z =
          let z' = z * z2
              n | neg       = flipSign z'
                | otherwise = z'
          in (n / d) : series (not neg) (d + two) z'
        z' = castRounding z
        z2 = z' * z'

-- | Compute the inverse sine of the argument to maximum precision using
-- series expansion.
seriesArcsin :: FinitePrecision p => Decimal p r -> ExtendedDecimal p
seriesArcsin z = infiniteSeries (+) (z' : series one two z' three)
  where series n d z i =
          let z' = z * z2
          in (n * z') / (d * i) : series (n * i) (d * (i + one)) z' (i + two)
        z' = castRounding z
        z2 = z' * z'

-- | Compute the inverse tangent of the (decimal) argument to maximum
-- precision.
arctan :: (FinitePrecision p, Rounding r) => Decimal p r -> ExtendedDecimal p
arctan z@Num {          } = arctan' (toRational z)
arctan   Inf { sign = s } = signFunc s halfPi
arctan   _                = qNaN

-- | Compute the inverse tangent of the (rational) argument to maximum
-- precision using a generalized continued fraction.
arctan' :: FinitePrecision p => Rational -> ExtendedDecimal p
arctan' z = continuedFraction m 0 $ (x, y) : partials 1 0 y
  where x = numerator   z
        y = denominator z
        m = fromInteger $ (42 * abs x) `div` y  -- estimated minimum # terms

        x2 = x * x
        ty = 2 * y

        -- [ (nx * nx, (n * 2 + 1) * y) | n <- [1..], let nx = n * x ]
        partials n a b =
          let a' = a + n * x2
              b' = b +     ty
          in (a', b') : partials (n + 2) a' b'

-- | Compute the inverse sine of the argument to maximum precision.
arcsin :: FinitePrecision p => Decimal p r -> ExtendedDecimal p
arcsin z = let z' = castUp z
           in castDown' $ two * arctan (z' / (one + sqrt (one - z' * z')))

-- | Compute the inverse cosine of the argument to maximum precision.
arccos :: FinitePrecision p => Decimal p r -> ExtendedDecimal p
arccos = castDown' . (halfPi -) . arcsin . castUp

-- | Compute π to maximum precision using the inverse sine series expansion.
seriesPi :: FinitePrecision p => ExtendedDecimal p
seriesPi = castDown' $ 6 * arcsin oneHalf

-- | Compute π to maximum precision using the best-known Machin-like formula.
machinPi :: FinitePrecision p => ExtendedDecimal p
machinPi = castDown' $ 16 * arctan' (1 % 5) - 4 * arctan' (1 % 239)

-- | Compute π to maximum precision using a generalized continued fraction
-- that converges linearly, adding at least three decimal digits of precision
-- per four terms.
cfPi :: FinitePrecision p => ExtendedDecimal p
cfPi = pi'
  where pi'    = continuedFraction m
                 0 $ (4, 1) : [ (n * n, n * 2 + 1) | n <- [1..] ]
        Just p = precision pi'
        m      = (p `div` 3) * 4

-- | Precomputed π to a precision of 50 digits
fastPi :: FinitePrecision p => ExtendedDecimal p
fastPi = 3.1415926535897932384626433832795028841971693993751

-- | Compute π/2 to maximum precision.
halfPi :: FinitePrecision p => ExtendedDecimal p
halfPi = castDown' $ pi * oneHalf

-- | Compute π/4 to maximum precision.
quarterPi :: FinitePrecision p => ExtendedDecimal p
quarterPi = castDown' $ pi * oneQuarter

-- | Compute (cos 𝛽, sin 𝛽) to maximum precision using Volder's algorithm
-- (CORDIC).
cordic :: FinitePrecision p
       => ExtendedDecimal p -> (ExtendedDecimal p, ExtendedDecimal p)
cordic beta@Num{}
  | beta >          halfPi = negatePair $ cordic (beta - pi)
  | beta < flipSign halfPi = negatePair $ cordic (beta + pi)
  | isZero beta            = (one, zero)
  | otherwise              = cordic' beta (one, zero) one angles

  where negatePair (x, y) = (flipSign x, flipSign y)

        angles = quarterPi : [ arctan' z | let half = 1 % 2
                                         , z <- iterate (* half) half ]

        cordic' beta v@(x, y) powerOfTwo ~(angle:angles)
          | v' == v   = (k * x, k * y)
          | otherwise = cordic' beta' v' powerOfTwo' angles
          where isNegBeta = isNegative beta
                beta'  | isNegBeta = beta + angle
                       | otherwise = beta - angle
                factor | isNegBeta = powerOfTwo { sign = Neg }
                       | otherwise = powerOfTwo
                v' = (x - factor * y, factor * x + y)
                powerOfTwo' = powerOfTwo * oneHalf

        -- K = lim {n→∞} K(n)
        -- K(n) = prod {i=0..n-1} 1 / sqrt (1 + 2^(-2 * i))
        k | p <= 50   = fastK
          | otherwise = seriesK
          where Just p  = precision k
                fastK   = 0.60725293500888125616944675250492826311239085215009
                seriesK = infiniteSeries (*)
                  [ recip $ sqrt (one + x) | x <- iterate (* oneQuarter) one ]

cordic _ = (qNaN, qNaN)

-- | Cast a number to a number with two additional digits of precision and
-- rounding half even.
castUp :: Precision p => Decimal p r -> ExtendedDecimal (PPlus1 (PPlus1 p))
castUp = coerce

-- | Cast a number with two additional digits of precision down to a number
-- with the desired precision, rounding half even.
castDown' :: Precision p
          => ExtendedDecimal (PPlus1 (PPlus1 p)) -> ExtendedDecimal p
castDown' = cast

-- | Cast a number with two additional digits of precision down to a number
-- with the desired precision, rounding half even, but returning a number type
-- with arbitrary rounding.
castDown :: (Precision p, Rounding r)
         => ExtendedDecimal (PPlus1 (PPlus1 p)) -> Decimal p r
castDown = castRounding . castDown'

-- | The constant 'pi' is precision-dependent.
instance (FinitePrecision p, Rounding r) => Floating (Decimal p r) where
  pi = castRounding pi'
    where pi' | p <= 50   = fastPi
              | otherwise = cfPi
          Just p = precision pi'

  exp = castRounding . evalOp . Op.exp
  log = castRounding . evalOp . Op.ln

  logBase b@Num{} x
    | b == ten = castRounding $ evalOp (Op.log10 x)
    | x == one = case b `compare` one of
        LT -> zero { sign = Neg }
        EQ -> qNaN
        GT -> zero
    | x == b && not (isZero b) = one
  logBase b x = evalOp (join $ Op.divide <$> Op.ln x <*> Op.ln b)

  x ** y = evalOp (x `Op.power` y)

  sqrt = castRounding . evalOp . Op.squareRoot

  sin  = castDown . snd                . cordic . castUp
  cos  = castDown . fst                . cordic . castUp
  tan  = castDown . uncurry (flip (/)) . cordic . castUp

  asin = castRounding . arcsin
  acos = castRounding . arccos
  atan = castRounding . arctan

  -- sinh x = let ex = exp x in (ex^2 - 1) / (2 * ex)
  sinh x = castDown . evalOp' $
    Op.exp x >>= \ex -> two `Op.multiply` ex >>= \tex ->
    ex `Op.multiply` ex >>= (`Op.subtract` one) >>= (`Op.divide` tex)
  -- cosh x = let ex = exp x in (ex^2 + 1) / (2 * ex)
  cosh x = castDown . evalOp' $
    Op.exp x >>= \ex -> two `Op.multiply` ex >>= \tex ->
    ex `Op.multiply` ex >>= (`Op.add` one) >>= (`Op.divide` tex)
  -- tanh x = let e2x = exp (2 * x) in (e2x - 1) / (e2x + 1)
  tanh x = castDown . evalOp' $
    two `Op.multiply` x >>= Op.exp >>= \e2x ->
    e2x `Op.subtract` one >>= \e2xm1 -> e2x `Op.add` one >>= (e2xm1 `Op.divide`)

  -- asinh x = log (x + sqrt (x^2 + 1))
  asinh x = castDown . evalOp' $ x `Op.multiply` x >>=
    (`Op.add` one) >>= Op.squareRoot >>= (x `Op.add`) >>= Op.ln
  -- acosh x = log (x + sqrt (x^2 - 1))
  acosh x = castDown . evalOp' $ x `Op.multiply` x >>=
    (`Op.subtract` one) >>= Op.squareRoot >>= (x `Op.add`) >>= Op.ln
  -- atanh x = log ((1 + x) / (1 - x)) / 2
  atanh x = castDown . evalOp' $ one `Op.add` x >>= \xp1 ->
    one `Op.subtract` x >>= (xp1 `Op.divide`) >>= Op.ln >>= Op.multiply oneHalf

instance (FinitePrecision p, Rounding r) => RealFloat (Decimal p r) where
  floatRadix  _ = 10
  floatDigits x = let Just p = precision x in p
  floatRange  x = let Just emin = eMin x
                      Just emax = eMax x
                  in (fromIntegral emin, fromIntegral emax)

  decodeFloat x = case x of
    Num { sign = s, coefficient = c, exponent = e }
      | c == 0    -> (0, 0)
      | otherwise -> (m, n)
      where m = signFunc s (fromIntegral $ c * 10^d)
            n = fromIntegral e - d
            d = floatDigits x - numDigits c
    Inf { sign = s                                } -> (special s 0, maxBound)
    NaN { sign = s, signaling = sig, payload = p  } -> (special s p, n)
      where n = minBound + fromEnum sig

    where special :: Sign -> Coefficient -> Integer
          special s v = signFunc s (pp + fromIntegral v)
          pp = 10 ^ floatDigits x :: Integer

  encodeFloat m n = x
    where x | am >= pp  = special
            | otherwise = cast Num { sign        = signMatch m
                                   , coefficient = fromInteger am
                                   , exponent    = fromIntegral n
                                   }
          special
            | n == maxBound     = Inf  { sign = signMatch m }
            | n == minBound     = qNaN { sign = signMatch m, payload = p }
            | otherwise         = sNaN { sign = signMatch m, payload = p }
            where p = fromInteger (am - pp)

          am = abs m              :: Integer
          pp = 10 ^ floatDigits x :: Integer

  isNaN x = case x of
    NaN{} -> True
    _     -> False

  isInfinite x = case x of
    Inf{} -> True
    _     -> False

  isDenormalized = isSubnormal

  isNegativeZero x = case x of
    Num { sign = Neg, coefficient = 0 } -> True
    _                                   -> False

  isIEEE _ = True

-- | The 'Bits' instance makes use of the logical operations from the
-- /General Decimal Arithmetic Specification/ using a /digit-wise/
-- representation of bits where the /sign/ is non-negative, the /exponent/ is
-- 0, and each decimal digit of the /coefficient/ must be either 0 or 1.
instance FinitePrecision p => Bits (Decimal p r) where
  x  .&.  y = evalOp (x `Op.and` y)
  x  .|.  y = evalOp (x `Op.or`  y)
  x `xor` y = evalOp (x `Op.xor` y)

  complement = evalOp . Op.invert

  shift  x i = evalOp $ Op.shift  x (fromIntegral i :: GeneralDecimal)
  rotate x i = evalOp $ Op.rotate x (fromIntegral i :: GeneralDecimal)

  zeroBits = zero

  bit i | i >= 0 && i < finiteBitSize x = x
        | otherwise                     = zeroBits
    where x = coerce zero { coefficient = 10 ^ i }

  testBit x@Num { sign = Pos, coefficient = c, exponent = 0 } i
    | i >= 0 && i < finiteBitSize x = (c `quot` 10 ^ i) `rem` 10 == 1
  testBit _ _ = False

  bitSizeMaybe = precision
  bitSize      = finiteBitSize

  isSigned _ = False

  popCount Num { sign = Pos, coefficient = c, exponent = 0 } = popCount' c 0
    where popCount' :: Coefficient -> Int -> Int
          popCount' 0 c = c
          popCount' x c = case d of
            0 -> popCount' x'         c
            1 -> popCount' x' $! succ c
            _ -> 0
            where (x', d) = x `quotRem` 10
  popCount _ = 0

instance FinitePrecision p => FiniteBits (Decimal p r) where
  finiteBitSize x = let Just p = precision x in p

instance NFData (Decimal p r) where
  rnf Num { sign = s, coefficient = c, exponent = e } =
    rnf s `seq` rnf c `seq` rnf e
  rnf Inf { sign = s } = rnf s
  rnf NaN { sign = s, signaling = sig, payload = p } =
    rnf s `seq` rnf sig `seq` rnf p

-- | A 'Decimal' representing the value zero
zero :: Decimal p r
zero = Num { sign        = Pos
           , coefficient = 0
           , exponent    = 0
           }

-- | A 'Decimal' representing the value ½
oneHalf :: Decimal p r
oneHalf = zero { coefficient = 5, exponent = -1 }

-- | A 'Decimal' representing the value ¼
oneQuarter :: Decimal p r
oneQuarter = zero { coefficient = 25, exponent = -2 }

-- | A 'Decimal' representing the value one
one :: Decimal p r
one = zero { coefficient = 1 }

-- | A 'Decimal' representing the value two
two :: Decimal p r
two = zero { coefficient = 2 }

-- | A 'Decimal' representing the value three
three :: Decimal p r
three = zero { coefficient = 3 }

-- | A 'Decimal' representing the value ten
ten :: Decimal p r
ten = zero { coefficient = 10 }

-- | A 'Decimal' representing the value negative one
negativeOne :: Decimal p r
negativeOne = one { sign = Neg }

-- | A 'Decimal' representing the value positive infinity
infinity :: Decimal p r
infinity = Inf { sign = Pos }

-- | A 'Decimal' representing undefined results
qNaN :: Decimal p r
qNaN = NaN { sign = Pos, signaling = False, payload = 0 }

-- | A signaling 'Decimal' representing undefined results
sNaN :: Decimal p r
sNaN = qNaN { signaling = True }

-- | Negate the given 'Decimal' by directly flipping its sign.
flipSign :: Decimal p r -> Decimal p r
flipSign n = n { sign = negateSign (sign n) }

-- | Cast a 'Decimal' to another precision and/or rounding algorithm,
-- immediately rounding if necessary to the new precision using the new
-- algorithm.
cast :: (Precision p, Rounding r) => Decimal a b -> Decimal p r
cast = evalOp . roundDecimal

-- | Cast a 'Decimal' to another rounding algorithm, maintaining the same
-- precision. No new rounding occurs.
castRounding :: Decimal p a -> Decimal p r
castRounding = coerce

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

-- | Is the sign of the given 'Decimal' positive?
isPositive :: Decimal p r -> Bool
isPositive n = case sign n of
  Pos -> True
  Neg -> False

-- | Is the sign of the given 'Decimal' negative?
isNegative :: Decimal p r -> Bool
isNegative n = case sign n of
  Neg -> True
  Pos -> False

-- | Does the given 'Decimal' represent a finite value?
isFinite :: Decimal p r -> Bool
isFinite Num{} = True
isFinite _     = False

-- | Does the given 'Decimal' represent the value zero?
isZero :: Decimal p r -> Bool
isZero Num { coefficient = 0 } = True
isZero _                       = False

-- | Is the given 'Decimal' normal?
isNormal :: Precision p => Decimal p r -> Bool
isNormal n
  | isFinite n && not (isZero n) = maybe True (adjustedExponent n >=) (eMin n)
  | otherwise                    = False

-- | Is the given 'Decimal' subnormal?
isSubnormal :: Precision p => Decimal p r -> Bool
isSubnormal n
  | isFinite n && not (isZero n) = maybe False (adjustedExponent n <) (eMin n)
  | otherwise                    = False

-- | Return @0@ or @1@ if the argument is 'False' or 'True', respectively.
-- This is basically an optimized @'toEnum' . 'fromEnum'@ and allows an
-- all-decimal usage of the operations from "Numeric.Decimal.Operation" that
-- return a 'Bool'.
fromBool :: Bool -> Decimal p r
fromBool False = zero
fromBool True  = one

-- | Return 'False' if the argument is zero or NaN, and 'True' otherwise.
toBool :: Decimal p r -> Bool
toBool Num { coefficient = c } = c /= 0
toBool NaN{}                   = False
toBool _                       = True

-- | Return @-1@, @0@, or @1@ if the argument is 'LT', 'EQ', or 'GT',
-- respectively. This allows an all-decimal usage of the operations from
-- "Numeric.Decimal.Operation" that return an 'Ordering'.
fromOrdering :: Ordering -> Decimal p r
fromOrdering LT = negativeOne
fromOrdering EQ = zero
fromOrdering GT = one

-- | Upper limit on the absolute value of the exponent
eLimit :: Precision p => p -> Maybe Exponent
eLimit = eMax -- ?

-- | Minimum value of the exponent for subnormal results
eTiny :: Precision p => p -> Maybe Exponent
eTiny n = (-) <$> eMin n <*> (fromIntegral . subtract 1 <$> precision n)

-- | Range of permissible exponent values
eRange :: Precision p => Decimal p r -> Maybe (Exponent, Exponent)
eRange n@Num { coefficient = c } = range <$> eLimit n
  where range :: Exponent -> (Exponent, Exponent)
        range lim = (-lim - clm1 + 1, lim - clm1)
        clength = numDigits c             :: Int
        clm1 = fromIntegral (clength - 1) :: Exponent
eRange _ = Nothing

adjustedExponent :: Decimal p r -> Exponent
adjustedExponent Num { coefficient = c, exponent = e } =
  e + fromIntegral (clength - 1)
  where clength = numDigits c :: Int
adjustedExponent _ = error "adjustedExponent: not a finite number"

integralValue :: Decimal a b -> Maybe Integer
integralValue Num { sign = s, coefficient = c, exponent = e }
  | e >= 0 = Just (signFunc s $ fromIntegral c * 10^e)
  | r == 0 = Just (signFunc s $ fromIntegral q)
  where (q, r) = c `quotRem` (10^(-e))
integralValue _ = Nothing
