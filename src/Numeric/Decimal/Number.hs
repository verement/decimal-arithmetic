
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

{- $setup
>>> :load Harness
-}

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

{- $doctest-Read
>>> fmap toRep (read "Just 123" :: Maybe GeneralDecimal)
Just (N (0,123,0))

>>> fmap toRep (read "Just (-12.0)" :: Maybe GeneralDecimal)
Just (N (1,120,-1))
-}

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

{- $doctest-Ord
prop> x > y ==> max x y == x && max y x == (x :: BasicDecimal)
prop> x < y ==> min x y == x && min y x == (x :: BasicDecimal)

prop> max x y == x ==> x >= y
prop> max x y == y ==> y >= x
prop> min x y == x ==> x <= y
prop> min x y == y ==> y <= x
-}

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

{- $doctest-Enum
>>> [0, 0.1 .. 2] :: [BasicDecimal]
[0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]

>>> [2, 1.9 .. 0] :: [BasicDecimal]
[2,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.2,1.1,1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.0]

>>> [1.7 .. 5.7] :: [BasicDecimal]
[1.7,2.7,3.7,4.7,5.7]
-}

instance (Precision p, Rounding r) => Num (Decimal p r) where
  x + y = evalOp (x `Op.add`      y)
  x - y = evalOp (x `Op.subtract` y)
  x * y = evalOp (x `Op.multiply` y)

  negate = evalOp . Op.minus
  abs    = evalOp . Op.abs

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

{- $doctest-Fractional
prop> (4.14 :: Decimal P2 RoundHalfUp)   == 4.1
prop> (4.15 :: Decimal P2 RoundHalfUp)   == 4.2
prop> (4.15 :: Decimal P2 RoundHalfDown) == 4.1
prop> (4.15 :: Decimal P2 RoundHalfEven) == 4.2
prop> (4.25 :: Decimal P2 RoundHalfEven) == 4.2
prop> (4.35 :: Decimal P2 RoundHalfEven) == 4.4
prop> (4.45 :: Decimal P2 RoundHalfEven) == 4.4
-}

instance (FinitePrecision p, Rounding r) => RealFrac (Decimal p r) where
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
infiniteSeries :: (FinitePrecision p, Rounding r)
               => [Decimal p r] -> Decimal p r
infiniteSeries = series zero
  where series n (x:xs)
          | n' == n   = n'
          | otherwise = series n' xs
          where n' = n + x
        series n []   = n

-- | Compute the arcsine of the argument to maximum precision using series
-- expansion.
arcsine :: (FinitePrecision p, Rounding r) => Decimal p r -> Decimal p r
arcsine x = infiniteSeries (x : series 1 2 x 3)
  where series n d x i =
          let x' = x * x2
          in (n * x') / (d * i) : series (n * i) (d * (i + one)) x' (i + two)
        x2 = x * x

-- | Compute π to maximum precision using the arcsine series expansion.
seriesPi :: FinitePrecision p => Decimal p RoundHalfEven
seriesPi = 6 * arcsine oneHalf

-- | Precomputed π to a precision of 50 digits
fastPi :: FinitePrecision p => Decimal p RoundHalfEven
fastPi = 3.1415926535897932384626433832795028841971693993751

-- | Cast a number with two additional digits of precision down to a number
-- with the desired precision.
castDown :: Precision p
         => Decimal (PPlus1 (PPlus1 p)) a -> Decimal p RoundHalfEven
castDown = cast

notyet :: String -> a
notyet = error . (++ ": not yet implemented")

-- | The trigonometric 'Floating' methods (other than the precision-dependent
-- constant 'pi') are not yet implemented.
instance (FinitePrecision p, Rounding r) => Floating (Decimal p r) where
  pi = castRounding pi'
    where pi' | p <= 50   = fastPi
              | otherwise = castDown seriesPi
          Just p = precision pi'

  exp = castRounding . evalOp . Op.exp
  log = castRounding . evalOp . Op.ln

  logBase 10 x = castRounding $ evalOp (Op.log10 x)
  logBase _  1 = zero
  logBase b  x = evalOp (join $ Op.divide <$> Op.ln x <*> Op.ln b)

  x ** y = evalOp (x `Op.power` y)

  sqrt = castRounding . evalOp . Op.squareRoot

  sin   = notyet "sin"
  cos   = notyet "cos"
  tan   = notyet "tan"

  asin  = notyet "asin"
  acos  = notyet "acos"
  atan  = notyet "atan"

  -- sinh x = let ex = exp x in (ex^2 - 1) / (2 * ex)
  sinh x = castRounding . castDown . evalOp' $
    Op.exp x >>= \ex -> two `Op.multiply` ex >>= \tex ->
    ex `Op.multiply` ex >>= (`Op.subtract` one) >>= (`Op.divide` tex)
  -- cosh x = let ex = exp x in (ex^2 + 1) / (2 * ex)
  cosh x = castRounding . castDown . evalOp' $
    Op.exp x >>= \ex -> two `Op.multiply` ex >>= \tex ->
    ex `Op.multiply` ex >>= (`Op.add` one) >>= (`Op.divide` tex)
  -- tanh x = let e2x = exp (2 * x) in (e2x - 1) / (e2x + 1)
  tanh x = castRounding . castDown . evalOp' $
    two `Op.multiply` x >>= Op.exp >>= \e2x ->
    e2x `Op.subtract` one >>= \e2xm1 -> e2x `Op.add` one >>= (e2xm1 `Op.divide`)

  -- asinh x = log (x + sqrt (x^2 + 1))
  asinh x = castRounding . castDown . evalOp' $ x `Op.multiply` x >>=
    (`Op.add` one) >>= Op.squareRoot >>= (x `Op.add`) >>= Op.ln
  -- acosh x = log (x + sqrt (x^2 - 1))
  acosh x = castRounding . castDown . evalOp' $ x `Op.multiply` x >>=
    (`Op.subtract` one) >>= Op.squareRoot >>= (x `Op.add`) >>= Op.ln
  -- atanh x = log ((1 + x) / (1 - x)) / 2
  atanh x = castRounding . castDown . evalOp' $ one `Op.add` x >>= \xp1 ->
    one `Op.subtract` x >>= (xp1 `Op.divide`) >>= Op.ln >>= Op.multiply oneHalf

{- $doctest-Floating
prop> realToFrac (pi :: ExtendedDecimal P16) == (pi :: Double)

prop> y >= 0 ==> (x :: BasicDecimal) ** fromInteger y == x ^ y

prop> isFinite x && x >= 0 ==> coefficient (sqrt (x * x) - (x :: ExtendedDecimal P16)) <= 1
-}

instance (FinitePrecision p, Rounding r) => RealFloat (Decimal p r) where
  floatRadix  _ = 10
  floatDigits x = let Just p = precision x in p
  floatRange  x = let Just emin = eMin x
                      Just emax = eMax x
                  in (fromIntegral emin, fromIntegral emax)

  decodeFloat x = case x of
    Num { sign = s, coefficient = c, exponent = e } -> (m, n)
      where m = signFunc s (fromIntegral c)
            n = fromIntegral e
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

{- $doctest-RealFloat
prop> uncurry encodeFloat (decodeFloat x) == (x :: BasicDecimal)
prop> isFinite x ==> significand x * fromInteger (floatRadix x) ^^ Prelude.exponent x == (x :: BasicDecimal)
prop> Prelude.exponent (0 :: BasicDecimal) == 0
prop> isFinite x && x /= 0 ==> Prelude.exponent (x :: BasicDecimal) == snd (decodeFloat x) + floatDigits x

prop> isNegativeZero (read "-0" :: BasicDecimal) == True
prop> isNegativeZero (read "+0" :: BasicDecimal) == False
prop> x /= 0 ==> isNegativeZero (x :: BasicDecimal) == False
-}

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

-- | A 'Decimal' representing the value one
one :: Decimal p r
one = zero { coefficient = 1 }

-- | A 'Decimal' representing the value two
two :: Decimal p r
two = zero { coefficient = 2 }

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
toBool Num { coefficient = c }
  | c == 0    = False
  | otherwise = True
toBool Inf{}  = True
toBool _      = False

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
