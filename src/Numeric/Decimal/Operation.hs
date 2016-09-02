
{- | The operations described in the /General Decimal Arithmetic Specification/
are provided here.

It is suggested to import this module qualified to avoid "Prelude" name
clashes:

> import qualified Numeric.Decimal.Operation as Op

Note that it is not usually necessary to import this module unless you want to
use operations unavailable through class methods, or you need precise control
over the handling of exceptional conditions. (See also
"Numeric.Decimal.Arithmetic".)
-}
module Numeric.Decimal.Operation
       ( -- * Arithmetic operations
         -- $arithmetic-operations

         -- ** General arithmetic
         add
       , subtract
       , multiply
       , divide
         -- divideInteger
         -- remainder
         -- remainderNear
       , power
       , squareRoot
       , fusedMultiplyAdd

         -- ** Exponential and logarithmic
       , exp
       , ln
       , log10

         -- ** Unary sign
       , plus
       , minus
       , abs

         -- ** Comparison
       , compare
       , compareSignal

       , min
       , max
       , minMagnitude
       , maxMagnitude

         -- ** Rounding and quantization

       , roundToIntegralValue
       , roundToIntegralExact
       , quantize
       , reduce

         -- nextMinus
         -- nextPlus
         -- nextToward

         -- * Miscellaneous operations
         -- $miscellaneous-operations

         -- ** Logic and shifting
         -- $logical-operations
       , and
       , or
       , xor
       , invert

       , shift
       , rotate

         -- ** Predicates
       , isZero
       , isSigned
       , isFinite
       , isInfinite
       , isNormal
       , isSubnormal
       , isNaN
       , isQNaN
       , isSNaN
       , isCanonical

         -- ** Total comparison and classification
       , compareTotal
       , compareTotalMagnitude

       , class', Class(..), Sign(..), NumberClass(..), NaNClass(..)

         -- ** Exponent manipulation
       , logb
       , scaleb
       , sameQuantum
       , radix

         -- ** Sign manipulation and conversion
       , copyAbs
       , copyNegate
       , copySign
       , copy

       , canonical
       ) where

import Prelude hiding (abs, and, compare, exp, exponent, isInfinite, isNaN,
                       max, min, or, subtract)
import qualified Prelude

import Control.Monad (join)
import Data.Bits (complement, setBit, testBit, zeroBits, (.&.), (.|.))
import Data.Coerce (coerce)
import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified Data.Bits as Bits

import Numeric.Decimal.Arithmetic
import Numeric.Decimal.Exception
import Numeric.Decimal.Number hiding (isFinite, isNormal, isSubnormal, isZero)
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

import qualified Numeric.Decimal.Number as Number

{- $setup
>>> :load Harness
-}

finitePrecision :: FinitePrecision p => Decimal p r -> Int
finitePrecision n = let Just p = precision n in p

roundingAlg :: Rounding r => Arith p r a -> RoundingAlgorithm
roundingAlg = rounding . arithRounding
  where arithRounding :: Arith p r a -> r
        arithRounding = undefined

result :: (Precision p, Rounding r) => Decimal p r -> Arith p r (Decimal p r)
result = roundDecimal  -- ...
--  | maybe False (numDigits c >) (precision r) = undefined

generalRules1 :: Decimal a b -> Arith p r (Decimal p r)
generalRules1 nan@NaN { signaling = False } = return (coerce nan)
generalRules1 x                             = invalidOperation x

generalRules2 :: Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
generalRules2 nan@NaN { signaling = True } _ = invalidOperation nan
generalRules2 _ nan@NaN { signaling = True } = invalidOperation nan
generalRules2 nan@NaN{} _                    = return (coerce nan)
generalRules2 _         nan@NaN{}            = return (coerce nan)
generalRules2 x         _                    = invalidOperation x

-- $arithmetic-operations
--
-- This section describes the arithmetic operations on, and some other
-- functions of, numbers, including subnormal numbers, negative zeros, and
-- special values (see also IEEE 754 §5 and §6).

{- $doctest-special-values
>>> op2 Op.add "Infinity" "1"
Infinity

>>> op2 Op.add "NaN" "1"
NaN

>>> op2 Op.add "NaN" "Infinity"
NaN

>>> op2 Op.subtract "1" "Infinity"
-Infinity

>>> op2 Op.multiply "-1" "Infinity"
-Infinity

>>> op2 Op.subtract "-0" "0"
-0

>>> op2 Op.multiply "-1" "0"
-0

>>> op2 Op.divide "1" "0"
Infinity

>>> op2 Op.divide "1" "-0"
-Infinity

>>> op2 Op.divide "-1" "0"
-Infinity
-}

-- | 'add' takes two operands. If either operand is a /special value/ then the
-- general rules apply.
--
-- Otherwise, the operands are added.
--
-- The result is then rounded to /precision/ digits if necessary, counting
-- from the most significant digit of the result.
add :: (Precision p, Rounding r)
    => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
add Num { sign = xs, coefficient = xc, exponent = xe }
    Num { sign = ys, coefficient = yc, exponent = ye } = sum

  where sum = result Num { sign = rs, coefficient = rc, exponent = re }
        rs | rc /= 0                       = if xac > yac then xs else ys
           | xs == Neg && ys == Neg        = Neg
           | xs /= ys &&
             roundingAlg sum == RoundFloor = Neg
           | otherwise                     = Pos
        rc | xs == ys  = xac + yac
           | xac > yac = xac - yac
           | otherwise = yac - xac
        re = Prelude.min xe ye
        (xac, yac) | xe == ye  = (xc, yc)
                   | xe >  ye  = (xc * 10^n, yc)
                   | otherwise = (xc, yc * 10^n)
          where n = Prelude.abs (xe - ye)

add inf@Inf { sign = xs } Inf { sign = ys }
  | xs == ys  = return (coerce inf)
  | otherwise = invalidOperation qNaN
add inf@Inf{} Num{} = return (coerce inf)
add Num{} inf@Inf{} = return (coerce inf)
add x y = generalRules2 x y

{- $doctest-add
>>> op2 Op.add "12" "7.00"
19.00

>>> op2 Op.add "1E+2" "1E+4"
1.01E+4
-}

-- | 'subtract' takes two operands. If either operand is a /special value/
-- then the general rules apply.
--
-- Otherwise, the operands are added after inverting the /sign/ used for the
-- second operand.
--
-- The result is then rounded to /precision/ digits if necessary, counting
-- from the most significant digit of the result.
subtract :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
subtract x = add x . flipSign

{- $doctest-subtract
>>> op2 Op.subtract "1.3" "1.07"
0.23

>>> op2 Op.subtract "1.3" "1.30"
0.00

>>> op2 Op.subtract "1.3" "2.07"
-0.77
-}

-- | 'minus' takes one operand, and corresponds to the prefix minus operator
-- in programming languages.
--
-- Note that the result of this operation is affected by context and may set
-- /flags/. The 'copyNegate' operation may be used instead of 'minus' if this
-- is not desired.
minus :: (Precision p, Rounding r) => Decimal a b -> Arith p r (Decimal p r)
minus x = zero { exponent = exponent x } `subtract` x

{- $doctest-minus
>>> op1 Op.minus "1.3"
-1.3

>>> op1 Op.minus "-1.3"
1.3
-}

-- | 'plus' takes one operand, and corresponds to the prefix plus operator in
-- programming languages.
--
-- Note that the result of this operation is affected by context and may set
-- /flags/.
plus :: (Precision p, Rounding r) => Decimal a b -> Arith p r (Decimal p r)
plus x = zero { exponent = exponent x } `add` x

{- $doctest-plus
>>> op1 Op.plus "1.3"
1.3

>>> op1 Op.plus "-1.3"
-1.3
-}

-- | 'multiply' takes two operands. If either operand is a /special value/
-- then the general rules apply. Otherwise, the operands are multiplied
-- together (“long multiplication”), resulting in a number which may be as
-- long as the sum of the lengths of the two operands.
--
-- The result is then rounded to /precision/ digits if necessary, counting
-- from the most significant digit of the result.
multiply :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
multiply Num { sign = xs, coefficient = xc, exponent = xe }
         Num { sign = ys, coefficient = yc, exponent = ye } = result rn

  where rn = Num { sign = rs, coefficient = rc, exponent = re }
        rs = xorSigns xs ys
        rc = xc * yc
        re = xe + ye

multiply Inf { sign = xs } Inf { sign = ys } =
  return Inf { sign = xorSigns xs ys }
multiply Inf { sign = xs } Num { sign = ys, coefficient = yc }
  | yc == 0   = invalidOperation qNaN
  | otherwise = return Inf { sign = xorSigns xs ys }
multiply Num { sign = xs, coefficient = xc } Inf { sign = ys }
  | xc == 0   = invalidOperation qNaN
  | otherwise = return Inf { sign = xorSigns xs ys }
multiply x y = generalRules2 x y

{- $doctest-multiply
>>> op2 Op.multiply "1.20" "3"
3.60

>>> op2 Op.multiply "7" "3"
21

>>> op2 Op.multiply "0.9" "0.8"
0.72

>>> op2 Op.multiply "0.9" "-0"
-0.0

>>> op2 Op.multiply "654321" "654321"
4.28135971E+11
-}

-- | 'exp' takes one operand. If the operand is a NaN then the general rules
-- for special values apply.
--
-- Otherwise, the result is /e/ raised to the power of the operand, with the
-- following cases:
--
-- * If the operand is −Infinity, the result is 0 and exact.
--
-- * If the operand is a zero, the result is 1 and exact.
--
-- * If the operand is +Infinity, the result is +Infinity and exact.
--
-- * Otherwise the result is inexact and will be rounded using the
-- 'RoundHalfEven' algorithm. The coefficient will have exactly /precision/
-- digits (unless the result is subnormal). These inexact results should be
-- correctly rounded, but may be up to 1 ulp (unit in last place) in error.
exp :: FinitePrecision p => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
exp x@Num { sign = s, coefficient = c }
  | c == 0    = return one
  | s == Neg  = subArith (maclaurin x { sign = Pos } >>= reciprocal) >>=
                subRounded >>= result
  | otherwise = subArith (maclaurin x) >>= subRounded >>= result

  where multiplyExact :: Decimal a b -> Decimal c d
                      -> Arith PInfinite RoundHalfEven
                         (Decimal PInfinite RoundHalfEven)
        multiplyExact = multiply

        maclaurin :: FinitePrecision p => Decimal a b
                  -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
        maclaurin x
          | adjustedExponent x >= 0 = subArith (subMaclaurin x) >>= subRounded
          | otherwise = sum one one one one
          where sum :: FinitePrecision p
                    => Decimal p RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
                sum s num den n = do
                  num' <- subArith (multiplyExact num x)
                  den' <- subArith (multiplyExact den n)
                  s' <- add s =<< divide num' den'
                  if s' == s then return s'
                    else sum s' num' den' =<< subArith (add n one)

        subMaclaurin :: FinitePrecision p => Decimal a b
                     -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
        subMaclaurin x = subArith (multiplyExact x oneHalf) >>= maclaurin >>=
          \r -> multiply r r

        subRounded :: Precision p
                   => Decimal (PPlus1 (PPlus1 p)) a
                   -> Arith p r (Decimal p RoundHalfEven)
        subRounded = subArith . roundDecimal

        result :: Decimal p a -> Arith p r (Decimal p a)
        result r = coerce <$> (raiseSignal Rounded =<< raiseSignal Inexact r')
          where r' = coerce r

exp n@Inf { sign = s }
  | s == Pos  = return (coerce n)
  | otherwise = return zero
exp x = coerce <$> generalRules1 x

{- $doctest-exp
>>> op1 Op.exp "-Infinity"
0

>>> op1 Op.exp "-1"
0.367879441

>>> op1 Op.exp "0"
1

>>> op1 Op.exp "1"
2.71828183

>>> op1 Op.exp "0.693147181"
2.00000000

>>> op1 Op.exp "+Infinity"
Infinity
-}

-- | 'fusedMultiplyAdd' takes three operands; the first two are multiplied
-- together, using 'multiply', with sufficient precision and exponent range
-- that the result is exact and unrounded. No /flags/ are set by the
-- multiplication unless one of the first two operands is a signaling NaN or
-- one is a zero and the other is an infinity.
--
-- Unless the multiplication failed, the third operand is then added to the
-- result of that multiplication, using 'add', under the current context.
--
-- In other words, @fusedMultiplyAdd x y z@ delivers a result which is @(x ×
-- y) + z@ with only the one, final, rounding.
fusedMultiplyAdd :: (Precision p, Rounding r)
                 => Decimal a b -> Decimal c d -> Decimal e f
                 -> Arith p r (Decimal p r)
fusedMultiplyAdd x y z =
  either raise (return . coerce) (exactMult x y) >>= add z

  where exactMult :: Rounding r => Decimal a b -> Decimal c d
                  -> Either (Exception PInfinite r) (Decimal PInfinite r)
        exactMult x y = evalArith (multiply x y) newContext

        raise :: Exception a r -> Arith p r (Decimal p r)
        raise e = raiseSignal (exceptionSignal e) (coerce $ exceptionResult e)

{- $doctest-fusedMultiplyAdd
>>> op3 Op.fusedMultiplyAdd "3" "5" "7"
22

>>> op3 Op.fusedMultiplyAdd "3" "-5" "7"
-8

>>> op3 Op.fusedMultiplyAdd "888565290" "1557.96930" "-86087.7578"
1.38435736E+12
-}

-- | 'ln' takes one operand. If the operand is a NaN then the general rules
-- for special values apply.
--
-- Otherwise, the operand must be a zero or positive, and the result is the
-- natural (base /e/) logarithm of the operand, with the following cases:
--
-- * If the operand is a zero, the result is −Infinity and exact.
--
-- * If the operand is +Infinity, the result is +Infinity and exact.
--
-- * If the operand equals one, the result is 0 and exact.
--
-- * Otherwise the result is inexact and will be rounded using the
-- 'RoundHalfEven' algorithm. The coefficient will have exactly /precision/
-- digits (unless the result is subnormal). These inexact results should be
-- correctly rounded, but may be up to 1 ulp (unit in last place) in error.
ln :: FinitePrecision p => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
ln x@Num { sign = s, coefficient = c, exponent = e }
  | c == 0   = return infinity { sign = Neg }
  | s == Pos = if e <= 0 && c == 10^(-e) then return zero
               else subArith (subLn x) >>= subRounded >>= result

  where subLn :: FinitePrecision p => Decimal a b
              -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
        subLn x = do
          let fe = fromIntegral (-(numDigits c - 1)) :: Exponent
              r  = fromIntegral (e - fe) :: Decimal PInfinite RoundHalfEven
          lnf <- taylorLn x { exponent = fe }
          add lnf =<< multiply r =<< ln10

        subRounded :: Precision p => Decimal (PPlus1 (PPlus1 p)) a
                   -> Arith p r (Decimal p RoundHalfEven)
        subRounded = subArith . roundDecimal

        result :: Decimal p a -> Arith p r (Decimal p a)
        result r = coerce <$> (raiseSignal Rounded =<< raiseSignal Inexact r')
          where r' = coerce r

ln n@Inf { sign = Pos } = return (coerce n)
ln x = coerce <$> generalRules1 x

taylorLn :: FinitePrecision p => Decimal a b
         -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
taylorLn x = do
  num <- x `subtract` one
  den <- x `add`      one
  multiply two =<< sum =<< num `divide` den

    where sum :: FinitePrecision p => Decimal p RoundHalfEven
              -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
          sum b = multiply b b >>= \b2 -> sum' b b b2 one

            where sum' :: FinitePrecision p
                       => Decimal p RoundHalfEven
                       -> Decimal p RoundHalfEven
                       -> Decimal p RoundHalfEven
                       -> Decimal PInfinite RoundHalfEven
                       -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
                  sum' s m b n = do
                    m' <- multiply m b
                    n' <- subArith (add n two)
                    s' <- add s =<< divide m' n'
                    if s' == s then return s' else sum' s' m' b n'

ln10 :: FinitePrecision p => Arith p r (Decimal p RoundHalfEven)
ln10 = getPrecision >>= \(Just p) ->
  if p <= 50 then return fastLn10 else slowLn10

  where fastLn10 :: FinitePrecision p => Decimal p RoundHalfEven
        fastLn10 = 2.3025850929940456840179914546843642076011014886288

        slowLn10 :: FinitePrecision p => Arith p r (Decimal p RoundHalfEven)
        slowLn10 = subArith (taylorLn ten) >>= subRound

          where subRound :: Precision p => Decimal (PPlus1 (PPlus1 p)) a
                         -> Arith p r (Decimal p RoundHalfEven)
                subRound = subArith . roundDecimal

{- $doctest-ln
>>> op1 Op.ln "0"
-Infinity

>>> op1 Op.ln "1.000"
0

>>> op1 Op.ln "2.71828183"
1.00000000

>>> op1 Op.ln "10"
2.30258509

>>> op1 Op.ln "+Infinity"
Infinity
-}

-- | 'log10' takes one operand. If the operand is a NaN then the general rules
-- for special values apply.
--
-- Otherwise, the operand must be a zero or positive, and the result is the
-- base 10 logarithm of the operand, with the following cases:
--
-- * If the operand is a zero, the result is −Infinity and exact.
--
-- * If the operand is +Infinity, the result is +Infinity and exact.
--
-- * If the operand equals an integral power of ten (including 10^0 and
-- negative powers) and there is sufficient /precision/ to hold the integral
-- part of the result, the result is an integer (with an exponent of 0) and
-- exact.
--
-- * Otherwise the result is inexact and will be rounded using the
-- 'RoundHalfEven' algorithm. The coefficient will have exactly /precision/
-- digits (unless the result is subnormal). These inexact results should be
-- correctly rounded, but may be up to 1 ulp (unit in last place) in error.
log10 :: FinitePrecision p => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
log10 x@Num { sign = s, coefficient = c, exponent = e }
  | c == 0   = return infinity { sign = Neg }
  | s == Pos = getPrecision >>= \prec -> case powerOfTen c e of
      Just p | maybe True (numDigits pc <=) prec -> return (fromInteger p)
        where pc = fromInteger (Prelude.abs p) :: Coefficient
      _ -> subArith (join $ divide <$> ln x <*> ln10) >>= result

  where powerOfTen :: Coefficient -> Exponent -> Maybe Integer
        powerOfTen c e
          | c == 10^d = Just (fromIntegral e + fromIntegral d)
          | otherwise = Nothing
          where d = numDigits c - 1 :: Int

        result :: Decimal p a -> Arith p r (Decimal p a)
        result r = coerce <$> (raiseSignal Rounded =<< raiseSignal Inexact r')
          where r' = coerce r

log10 n@Inf { sign = Pos } = return (coerce n)
log10 x = coerce <$> generalRules1 x

{- $doctest-log10
>>> op1 Op.log10 "0"
-Infinity

>>> op1 Op.log10 "0.001"
-3

>>> op1 Op.log10 "1.000"
0

>>> op1 Op.log10 "2"
0.301029996

>>> op1 Op.log10 "10"
1

>>> op1 Op.log10 "70"
1.84509804

>>> op1 Op.log10 "+Infinity"
Infinity
-}

-- | 'divide' takes two operands. If either operand is a /special value/ then
-- the general rules apply.
--
-- Otherwise, if the divisor is zero then either the Division undefined
-- condition is raised (if the dividend is zero) and the result is NaN, or the
-- Division by zero condition is raised and the result is an Infinity with a
-- sign which is the exclusive or of the signs of the operands.
--
-- Otherwise, a “long division” is effected.
--
-- The result is then rounded to /precision/ digits, if necessary, according
-- to the /rounding/ algorithm and taking into account the remainder from the
-- division.
divide :: (FinitePrecision p, Rounding r)
       => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
divide dividend@Num{ sign = xs } Num { coefficient = 0, sign = ys }
  | Number.isZero dividend = divisionUndefined
  | otherwise              = divisionByZero infinity { sign = xorSigns xs ys }
divide Num { sign = xs, coefficient = xc, exponent = xe }
       Num { sign = ys, coefficient = yc, exponent = ye } = quotient

  where quotient = result =<< answer
        rn = Num { sign = rs, coefficient = rc, exponent = re }
        rs = xorSigns xs ys
        (rc, rem, dv, adjust) = longDivision xc yc (finitePrecision rn)
        re = xe - (ye + adjust)
        answer
          | rem == 0  = return rn
          | otherwise = roundDecimal $ case (rem * 2) `Prelude.compare` dv of
              LT -> rn { coefficient = rc * 10 + 1, exponent = re - 1 }
              EQ -> rn { coefficient = rc * 10 + 5, exponent = re - 1 }
              GT -> rn { coefficient = rc * 10 + 9, exponent = re - 1 }

divide Inf{} Inf{} = invalidOperation qNaN
divide Inf { sign = xs } Num { sign = ys } =
  return Inf { sign = xorSigns xs ys }
divide Num { sign = xs } Inf { sign = ys } =
  return zero { sign = xorSigns xs ys }
divide x y = generalRules2 x y

{- $doctest-divide
>>> op2 Op.divide "1" "3"
0.333333333

>>> op2 Op.divide "2" "3"
0.666666667

>>> op2 Op.divide "5" "2"
2.5

>>> op2 Op.divide "1" "10"
0.1

>>> op2 Op.divide "12" "12"
1

>>> op2 Op.divide "8.00" "2"
4.00

>>> op2 Op.divide "2.400" "2.0"
1.20

>>> op2 Op.divide "1000" "100"
10

>>> op2 Op.divide "1000" "1"
1000

>>> op2 Op.divide "2.40E+6" "2"
1.20E+6
-}

type Dividend  = Coefficient
type Divisor   = Coefficient
type Quotient  = Coefficient
type Remainder = Dividend

longDivision :: Dividend -> Divisor -> Int
             -> (Quotient, Remainder, Divisor, Exponent)
longDivision 0  dv _ = (0, 0, dv, 0)
longDivision dd dv p = step1 dd dv 0

  where step1 :: Dividend -> Divisor -> Exponent
              -> (Quotient, Remainder, Divisor, Exponent)
        step1 dd dv adjust
          | dd <       dv = step1 (dd * 10)  dv       (adjust + 1)
          | dd >= 10 * dv = step1  dd       (dv * 10) (adjust - 1)
          | otherwise     = step2  dd        dv        adjust

        step2 :: Dividend -> Divisor -> Exponent
              -> (Quotient, Remainder, Divisor, Exponent)
        step2 = step3 0

        step3 :: Quotient -> Dividend -> Divisor -> Exponent
              -> (Quotient, Remainder, Divisor, Exponent)
        step3 r dd dv adjust
          | dv <= dd                 = step3 (r +  1) (dd - dv) dv  adjust
          | (dd == 0 && adjust >= 0) ||
            numDigits r == p         = step4  r        dd       dv  adjust
          | otherwise                = step3 (r * 10) (dd * 10) dv (adjust + 1)

        step4 :: Quotient -> Remainder -> Divisor -> Exponent
              -> (Quotient, Remainder, Divisor, Exponent)
        step4 = (,,,)

reciprocal :: (FinitePrecision p, Rounding r)
           => Decimal a b -> Arith p r (Decimal p r)
reciprocal = divide one

-- | 'abs' takes one operand. If the operand is negative, the result is the
-- same as using the 'minus' operation on the operand. Otherwise, the result
-- is the same as using the 'plus' operation on the operand.
--
-- Note that the result of this operation is affected by context and may set
-- /flags/. The 'copyAbs' operation may be used if this is not desired.
abs :: (Precision p, Rounding r) => Decimal a b -> Arith p r (Decimal p r)
abs x
  | isNegative x = minus x
  | otherwise    = plus  x

{- $doctest-abs
>>> op1 Op.abs "2.1"
2.1

>>> op1 Op.abs "-100"
100

>>> op1 Op.abs "101.5"
101.5

>>> op1 Op.abs "-101.5"
101.5
-}

-- | 'compare' takes two operands and compares their values numerically. If
-- either operand is a /special value/ then the general rules apply. No flags
-- are set unless an operand is a signaling NaN.
--
-- Otherwise, the operands are compared, returning @'Right' 'LT'@ if the first
-- is less than the second, @'Right' 'EQ'@ if they are equal, or @'Right'
-- 'GT'@ if the first is greater than the second.
--
-- A 'Left' value is returned if the result is NaN, indicating an “unordered”
-- comparison (see IEEE 754 §5.11).
compare :: Decimal a b -> Decimal c d
        -> Arith p r (Either (Decimal p r) Ordering)
compare x@Num{} y@Num{} = nzp <$> subArith (subtract' xn yn)

  where subtract' :: Decimal a b -> Decimal c d
                  -> Arith PInfinite RoundHalfEven
                     (Decimal PInfinite RoundHalfEven)
        subtract' = subtract

        (xn, yn) | sign x /= sign y = (either id fromOrdering $ nzp x,
                                       either id fromOrdering $ nzp y)
                 | otherwise        = (x, y)

        nzp :: Decimal a b -> Either (Decimal p r) Ordering
        nzp Num { sign = s, coefficient = c }
          | c == 0    = Right EQ
          | s == Pos  = Right GT
          | otherwise = Right LT
        nzp Inf { sign = s } = case s of
          Pos -> Right GT
          Neg -> Right LT
        nzp n = Left (coerce n)

compare Inf { sign = xs } Inf { sign = ys } = return $ case (xs, ys) of
  (Pos, Neg) -> Right GT
  (Neg, Pos) -> Right LT
  _          -> Right EQ
compare Inf { sign = xs } Num { } = return $ case xs of
  Pos -> Right GT
  Neg -> Right LT
compare Num { } Inf { sign = ys } = return $ case ys of
  Pos -> Right LT
  Neg -> Right GT
compare x y = Left <$> generalRules2 x y

{- $doctest-compare
>>> either id fromOrdering $ op2 Op.compare "2.1" "3"
-1

>>> either id fromOrdering $ op2 Op.compare "2.1" "2.1"
0

>>> either id fromOrdering $ op2 Op.compare "2.1" "2.10"
0

>>> either id fromOrdering $ op2 Op.compare "3" "2.1"
1

>>> either id fromOrdering $ op2 Op.compare "2.1" "-3"
1

>>> either id fromOrdering $ op2 Op.compare "-3" "2.1"
-1
-}

-- | 'compareSignal' takes two operands and compares their values
-- numerically. This operation is identical to 'compare', except that if
-- neither operand is a signaling NaN then any quiet NaN operand is treated as
-- though it were a signaling NaN. (That is, all NaNs signal, with signaling
-- NaNs taking precedence over quiet NaNs.)
compareSignal :: Decimal a b -> Decimal c d
              -> Arith p r (Either (Decimal p r) Ordering)
compareSignal x@NaN { signaling = True } y =     x `compare`     y
compareSignal x y@NaN { signaling = True } =     x `compare`     y
compareSignal x y                          = q2s x `compare` q2s y

  where q2s :: Decimal p r -> Decimal p r
        q2s nan@NaN{} = nan { signaling = True }
        q2s x         = x

-- | 'max' takes two operands, compares their values numerically, and returns
-- the maximum. If either operand is a NaN then the general rules apply,
-- unless one is a quiet NaN and the other is numeric, in which case the
-- numeric operand is returned.
max :: Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
max x y = snd <$> minMax id x y

{- $doctest-max
>>> op2 Op.max "3" "2"
3

>>> op2 Op.max "-10" "3"
3

>>> op2 Op.max "1.0" "1"
1

>>> op2 Op.max "7" "NaN"
7
-}

-- | 'maxMagnitude' takes two operands and compares their values numerically
-- with their /sign/ ignored and assumed to be 0.
--
-- If, without signs, the first operand is the larger then the original first
-- operand is returned (that is, with the original sign). If, without signs,
-- the second operand is the larger then the original second operand is
-- returned. Otherwise the result is the same as from the 'max' operation.
maxMagnitude :: Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
maxMagnitude x y = snd <$> minMax withoutSign x y

-- | 'min' takes two operands, compares their values numerically, and returns
-- the minimum. If either operand is a NaN then the general rules apply,
-- unless one is a quiet NaN and the other is numeric, in which case the
-- numeric operand is returned.
min :: Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
min x y = fst <$> minMax id x y

{- $doctest-min
>>> op2 Op.min "3" "2"
2

>>> op2 Op.min "-10" "3"
-10

>>> op2 Op.min "1.0" "1"
1.0

>>> op2 Op.min "7" "NaN"
7
-}

-- | 'minMagnitude' takes two operands and compares their values numerically
-- with their /sign/ ignored and assumed to be 0.
--
-- If, without signs, the first operand is the smaller then the original first
-- operand is returned (that is, with the original sign). If, without signs,
-- the second operand is the smaller then the original second operand is
-- returned. Otherwise the result is the same as from the 'min' operation.
minMagnitude :: Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
minMagnitude x y = fst <$> minMax withoutSign x y

-- | Ordering function for 'min', 'minMagnitude', 'max', and 'maxMagnitude':
-- returns the original arguments as (smaller, larger) when the given function
-- is applied to them.
minMax :: (Decimal a b -> Decimal a b) -> Decimal a b -> Decimal a b
       -> Arith p r (Decimal a b, Decimal a b)
minMax _ x@Num{}                       NaN { signaling = False } = return (x, x)
minMax _ x@Inf{}                       NaN { signaling = False } = return (x, x)
minMax _   NaN { signaling = False } y@Num{}                     = return (y, y)
minMax _   NaN { signaling = False } y@Inf{}                     = return (y, y)

minMax f x y = f x `compare` f y >>= \c -> return $ case c of
  Right LT -> (x, y)
  Right GT -> (y, x)
  Right EQ -> case (sign x, sign y) of
    (Neg, Pos) -> (x, y)
    (Pos, Neg) -> (y, x)
    (Pos, Pos) -> case (x, y) of
      (Num { exponent = xe }, Num { exponent = ye }) | xe > ye -> (y, x)
      _ -> (x, y)
    (Neg, Neg) -> case (x, y) of
      (Num { exponent = xe }, Num { exponent = ye }) | xe < ye -> (y, x)
      _ -> (x, y)
  Left nan -> let nan' = coerce nan in (nan', nan')

withoutSign :: Decimal p r -> Decimal p r
withoutSign n = n { sign = Pos }

-- | 'power' takes two operands, and raises a number (the left-hand operand)
-- to a power (the right-hand operand). If either operand is a /special value/
-- then the general rules apply, except as stated below.
--
-- The following rules apply:
--
-- * If both operands are zero, or if the left-hand operand is less than zero
-- and the right-hand operand does not have an integral value or is infinite,
-- an Invalid operation condition is raised, the result is NaN, and the
-- following rules do not apply.
--
-- * If the left-hand operand is infinite, the result will be exact and will
-- be infinite if the right-hand side is positive, 1 if the right-hand side is
-- a zero, and 0 if the right-hand side is negative.
--
-- * If the left-hand operand is a zero, the result will be exact and will be
-- infinite if the right-hand side is negative or 0 if the right-hand side is
-- positive.
--
-- * If the right-hand operand is a zero, the result will be 1 and exact.
--
-- * In cases not covered above, the result will be inexact unless the
-- right-hand side has an integral value and the result is finite and can be
-- expressed exactly within /precision/ digits. In this latter case, if the
-- result is unrounded then its exponent will be that which would result if
-- the operation were calculated by repeated multiplication (if the second
-- operand is negative then the reciprocal of the first operand is used, with
-- the absolute value of the second operand determining the multiplications).
--
-- * Inexact finite results should be correctly rounded, but may be up to 1
-- ulp (unit in last place) in error.
--
-- * The /sign/ of the result will be 1 only if the right-hand side has an
-- integral value and is odd (and is not infinite) and also the /sign/ of the
-- left-hand side is 1. In all other cases, the /sign/ of the result will be
-- 0.
power :: (FinitePrecision p, Rounding r)
      => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
power x@Num { coefficient = 0 } y@Num{}
  | Number.isZero y     = invalidOperation qNaN
  | Number.isNegative y = divisionByZero infinity { sign = powerSign x y }
  | otherwise           = return zero     { sign = powerSign x y }
power x@Num{} y@Num{} = case integralValue y of
  Just i  | i < 0               -> reciprocal x >>= \rx -> integralPower rx (-i)
          | otherwise           ->                         integralPower  x   i
  Nothing | Number.isPositive x -> ln x >>= multiply y >>= fmap coerce . exp
          | otherwise           -> invalidOperation qNaN
power x@Num{} y@Inf{}
  | Number.isPositive x = return $ case sign y of
      Pos -> infinity
      Neg -> zero
  | otherwise           = invalidOperation qNaN
power x@Inf{} y@Num{}
  | Number.isZero y     = return one
  | Number.isPositive y = return infinity { sign = powerSign x y }
  | otherwise           = return zero     { sign = powerSign x y }
power Inf{} Inf { sign = s }
  | s == Pos            = return infinity
  | otherwise           = return zero
power x y = generalRules2 x y

powerSign :: Decimal a b -> Decimal c d -> Sign
powerSign x y
  | Number.isNegative x && fromMaybe False (odd <$> integralValue y) = Neg
  | otherwise                                                        = Pos

integralPower :: (Precision p, Rounding r)
              => Decimal a b -> Integer -> Arith p r (Decimal p r)
integralPower b e = integralPower' (return b) e one
  where integralPower' :: (Precision p, Rounding r)
                       => Arith p r (Decimal a b) -> Integer -> Decimal p r
                       -> Arith p r (Decimal p r)
        integralPower' _  0 r = return r
        integralPower' mb e r
          | odd e     = mb >>= \b -> multiply r b >>=
                        integralPower'              (multiply b b) e'
          | otherwise = integralPower' (mb >>= \b -> multiply b b) e' r
          where e' = e `div` 2

{- $doctest-power
>>> op2 Op.power "2" "3"
8

>>> op2 Op.power "-2" "3"
-8

>>> op2 Op.power "2" "-3"
0.125

>>> op2 Op.power "1.7" "8"
69.7575744

>>> op2 Op.power "10" "0.301029996"
2.00000000

>>> op2 Op.power "Infinity" "-1"
0

>>> op2 Op.power "Infinity" "0"
1

>>> op2 Op.power "Infinity" "1"
Infinity

>>> op2 Op.power "-Infinity" "-1"
-0

>>> op2 Op.power "-Infinity" "0"
1

>>> op2 Op.power "-Infinity" "1"
-Infinity

>>> op2 Op.power "-Infinity" "2"
Infinity

>>> op2 Op.power "0" "0"
NaN
-}

-- | 'quantize' takes two operands. If either operand is a /special value/
-- then the general rules apply, except that if either operand is infinite and
-- the other is finite an Invalid operation condition is raised and the result
-- is NaN, or if both are infinite then the result is the first operand.
--
-- Otherwise (both operands are finite), 'quantize' returns the number which
-- is equal in value (except for any rounding) and sign to the first
-- (left-hand) operand and which has an /exponent/ set to be equal to the
-- exponent of the second (right-hand) operand.
--
-- The /coefficient/ of the result is derived from that of the left-hand
-- operand. It may be rounded using the current /rounding/ setting (if the
-- /exponent/ is being increased), multiplied by a positive power of ten (if
-- the /exponent/ is being decreased), or is unchanged (if the /exponent/ is
-- already equal to that of the right-hand operand).
--
-- Unlike other operations, if the length of the /coefficient/ after the
-- quantize operation would be greater than /precision/ then an Invalid
-- operation condition is raised. This guarantees that, unless there is an
-- error condition, the /exponent/ of the result of a quantize is always equal
-- to that of the right-hand operand.
--
-- Also unlike other operations, quantize will never raise Underflow, even if
-- the result is subnormal and inexact.
quantize :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
quantize x@Num { coefficient = xc, exponent = xe } Num { exponent = ye }
  | xe > ye   = result x { coefficient = xc * 10^(xe - ye), exponent = ye }
  | xe < ye   = rc >>= \c -> result x { coefficient = c, exponent = ye }
  | otherwise = result x

  where result :: Precision p => Decimal a b -> Arith p r (Decimal p r)
        result x = getPrecision >>= \p -> case numDigits (coefficient x) of
          n | maybe False (n >) p -> invalidOperation qNaN
          _                       -> return (coerce x)

        rc :: Rounding r => Arith p r Coefficient
        rc = let b      = 10^(ye - xe)
                 (q, r) = xc `quotRem` b
             in getRounder >>= \rounder -> return (rounder (sign x) r b q)

quantize Num{}   Inf{} = invalidOperation qNaN
quantize Inf{}   Num{} = invalidOperation qNaN
quantize x@Inf{} Inf{} = return (coerce x)
quantize x y = generalRules2 x y

{- $doctest-quantize
>>> op2 Op.quantize "2.17" "0.001"
2.170

>>> op2 Op.quantize "2.17" "0.01"
2.17

>>> op2 Op.quantize "2.17" "0.1"
2.2

>>> op2 Op.quantize "2.17" "1e+0"
2

>>> op2 Op.quantize "2.17" "1e+1"
0E+1

>>> op2 Op.quantize "-Inf" "Infinity"
-Infinity

>>> op2 Op.quantize "2" "Infinity"
NaN

>>> op2 Op.quantize "-0.1" "1"
-0

>>> op2 Op.quantize "-0" "1e+5"
-0E+5

>>> op2 Op.quantize "+35236450.6" "1e-2"
NaN

>>> op2 Op.quantize "-35236450.6" "1e-2"
NaN

>>> op2 Op.quantize "217" "1e-1"
217.0

>>> op2 Op.quantize "217" "1e+0"
217

>>> op2 Op.quantize "217" "1e+1"
2.2E+2

>>> op2 Op.quantize "217" "1e+2"
2E+2
-}

-- | 'reduce' takes one operand. It has the same semantics as the 'plus'
-- operation, except that if the final result is finite it is reduced to its
-- simplest form, with all trailing zeros removed and its sign preserved.
reduce :: (Precision p, Rounding r) => Decimal a b -> Arith p r (Decimal p r)
reduce n = reduce' <$> plus n
  where reduce' n@Num { coefficient = c, exponent = e }
          | c == 0 =         n {                  exponent = 0     }
          | r == 0 = reduce' n { coefficient = q, exponent = e + 1 }
          where (q, r) = c `quotRem` 10
        reduce' n = n

{- $doctest-reduce
>>> op1 Op.reduce "2.1"
2.1

>>> op1 Op.reduce "-2.0"
-2

>>> op1 Op.reduce "1.200"
1.2

>>> op1 Op.reduce "-120"
-1.2E+2

>>> op1 Op.reduce "120.00"
1.2E+2

>>> op1 Op.reduce "0.00"
0
-}

-- | 'roundToIntegralExact' takes one operand. If the operand is a
-- /special value/, or the exponent of the operand is non-negative, then the
-- result is the same as the operand (unless the operand is a signaling NaN,
-- as usual).
--
-- Otherwise (the operand has a negative exponent) the result is the same as
-- using the 'quantize' operation using the given operand as the
-- left-hand-operand, 1E+0 as the right-hand-operand, and the precision of the
-- operand as the /precision/ setting. The rounding mode is taken from the
-- context, as usual.
roundToIntegralExact :: (Precision a, Rounding r)
                     => Decimal a b -> Arith p r (Decimal a r)
roundToIntegralExact x@Num { exponent = e }
  | e >= 0    = return (coerce x)
  | otherwise =
      let (Right r, context) = runArith (quantize x one) newContext
          quantizeFlags = flags context

          maybeRaise :: Signal -> Decimal a r -> Arith p r (Decimal a r)
          maybeRaise sig
            | sig `signalMember` quantizeFlags =
                fmap coerce . raiseSignal sig . coerce
            | otherwise = return

      in maybeRaise Inexact r >> maybeRaise Rounded r

roundToIntegralExact x@Inf{} = return (coerce x)
roundToIntegralExact x = coerce <$> generalRules1 x

{- $doctest-roundToIntegralExact
>>> op1 Op.roundToIntegralExact "2.1"
2

>>> op1 Op.roundToIntegralExact "100"
100

>>> op1 Op.roundToIntegralExact "100.0"
100

>>> op1 Op.roundToIntegralExact "101.5"
102

>>> op1 Op.roundToIntegralExact "-101.5"
-102

>>> op1 Op.roundToIntegralExact "10E+5"
1.0E+6

>>> op1 Op.roundToIntegralExact "7.89E+77"
7.89E+77

>>> op1 Op.roundToIntegralExact "-Inf"
-Infinity
-}

-- | 'roundToIntegralValue' takes one operand. It is identical to the
-- 'roundToIntegralExact' operation except that the 'Inexact' and 'Rounded'
-- flags are never set even if the operand is rounded (that is, the operation
-- is quiet unless the operand is a signaling NaN).
roundToIntegralValue :: (Precision a, Rounding r)
                     => Decimal a b -> Arith p r (Decimal a r)
roundToIntegralValue x@Num { exponent = e }
  | e >= 0    = return (coerce x)
  | otherwise = subArith (quantize x one)
roundToIntegralValue x@Inf{} = return (coerce x)
roundToIntegralValue x = coerce <$> generalRules1 x

-- | 'squareRoot' takes one operand. If the operand is a /special value/ then
-- the general rules apply.
--
-- Otherwise, the ideal exponent of the result is defined to be half the
-- exponent of the operand (rounded to an integer, towards −Infinity, if
-- necessary) and then:
--
-- If the operand is less than zero an Invalid operation condition is raised.
--
-- If the operand is greater than zero, the result is the square root of the
-- operand. If no rounding is necessary (the exact result requires /precision/
-- digits or fewer) then the coefficient and exponent giving the correct value
-- and with the exponent closest to the ideal exponent is used. If the result
-- must be inexact, it is rounded using the 'RoundHalfEven' algorithm and the
-- coefficient will have exactly /precision/ digits (unless the result is
-- subnormal), and the exponent will be set to maintain the correct value.
--
-- Otherwise (the operand is equal to zero), the result will be the zero with
-- the same sign as the operand and with the ideal exponent.
squareRoot :: FinitePrecision p
           => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
squareRoot n@Num { sign = s, coefficient = c, exponent = e }
  | c == 0   = return n { exponent = idealExp }
  | s == Pos = subResult >>= subRounded >>= result

  where idealExp = e `div` 2 :: Exponent

        reduced :: Decimal p r -> Decimal p r
        reduced n@Num { coefficient = c, exponent = e }
          | e < idealExp = case bd of
              Just (b, (q, _)) -> n { coefficient = q, exponent = e + b }
              Nothing          -> n
          | e > idealExp = n { coefficient = c * 10^d, exponent = idealExp }
          where d  = Prelude.abs (e - idealExp)
                bd = find (\(_, (_, r)) -> r == 0) ds
                ds = map (\d -> (d, c `quotRem` (10^d))) [d, d - 1 .. 1]
        reduced n = n

        subResult :: FinitePrecision p
                  => Arith p r (Decimal (PPlus1 (PPlus1 p)) RoundHalfEven)
        subResult = subArith (babylonian approx)

        subRounded :: Precision p
                   => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
        subRounded = subArith . roundDecimal

        exactness :: Decimal a b -> Arith p r
                     (Either (Decimal p r) Ordering)
        exactness r = subArith (multiply' r r) >>= compare n
          where multiply' :: Decimal a b -> Decimal c d
                          -> Arith PInfinite RoundHalfEven
                             (Decimal PInfinite RoundHalfEven)
                multiply' = multiply

        result :: Decimal p a -> Arith p r (Decimal p a)
        result r = exactness r >>= \e -> case e of
          Right EQ -> return (reduced r)
          _ -> let r' = coerce r
               in coerce <$> (raiseSignal Rounded =<< raiseSignal Inexact r')

        approx :: Decimal p r
        approx | even ae   = n { coefficient = 2, exponent =  ae      `quot` 2 }
               | otherwise = n { coefficient = 6, exponent = (ae - 1) `quot` 2 }
          where ae = adjustedExponent n

        babylonian :: FinitePrecision p => Decimal p RoundHalfEven
                   -> Arith p RoundHalfEven (Decimal p RoundHalfEven)
        babylonian x = do
          x' <- multiply oneHalf =<< add x =<< n `divide` x
          if x' == x then return x' else babylonian x'

squareRoot n@Inf { sign = Pos } = return (coerce n)
squareRoot x = coerce <$> generalRules1 x

{- $doctest-squareRoot
>>> op1 Op.squareRoot "0"
0

>>> op1 Op.squareRoot "-0"
-0

The following example is a corrected version of that found in the
specification; confirmed with Mike Cowlishaw on 2016-08-02.
>>> op1 Op.squareRoot "0.39"
0.624499800

>>> op1 Op.squareRoot "100"
10

>>> op1 Op.squareRoot "1"
1

>>> op1 Op.squareRoot "1.0"
1.0

>>> op1 Op.squareRoot "1.00"
1.0

>>> op1 Op.squareRoot "7"
2.64575131

>>> op1 Op.squareRoot "10"
3.16227766
-}

-- $miscellaneous-operations
--
-- This section describes miscellaneous operations on decimal numbers,
-- including non-numeric comparisons, sign and other manipulations, and
-- logical operations.
--
-- Some operations return a boolean value described as 0 or 1 in the
-- /General Decimal Arithmetic Specification/, but which is returned as a
-- 'Bool' in this implementation. These values can be converted to 'Decimal'
-- via 'fromBool'.
--
-- Similarly, the total ordering operations return an 'Ordering' value in this
-- implementation, but can be converted to 'Decimal' via 'fromOrdering'.

data Logical = Logical { bits :: Integer, bitLength :: Int }

toLogical :: Decimal a b -> Maybe Logical
toLogical Num { sign = Pos, coefficient = c, exponent = 0 } =
  getBits c Logical { bits = zeroBits, bitLength = 0 }

  where getBits :: Coefficient -> Logical -> Maybe Logical
        getBits 0 g = return g
        getBits c g@Logical { bits = b, bitLength = l } = case d of
          0 -> getBits c' g {                    bitLength = succ l }
          1 -> getBits c' g { bits = setBit b l, bitLength = succ l }
          _ -> Nothing
          where (c', d) = c `quotRem` 10

toLogical _ = Nothing

fromLogical :: Logical -> Decimal a b
fromLogical Logical { bits = b, bitLength = l } =
  Num { sign = Pos, coefficient = fromBits 0 1 0, exponent = 0 }

  where fromBits :: Int -> Coefficient -> Coefficient -> Coefficient
        fromBits i r c
          | i == l      = c
          | testBit b i = fromBits i' r' (c + r)
          | otherwise   = fromBits i' r'  c
          where i' = succ i
                r' = r * 10

-- $logical-operations
--
-- The logical operations ('and', 'or', 'xor', and 'invert') take
-- /logical operands/, which are finite numbers with a /sign/ of 0, an
-- /exponent/ of 0, and a /coefficient/ whose digits must all be either 0 or
-- 1. The length of the result will be at most /precision/ digits (all of
-- which will be either 0 or 1); operands are truncated on the left or padded
-- with zeros on the left as necessary. The result of a logical operation is
-- never rounded and the only /flag/ that might be set is 'InvalidOperation'
-- (set if an operand is not a valid logical operand).

-- | 'and' is a logical operation which takes two logical operands. The result
-- is the digit-wise /and/ of the two operands; each digit of the result is
-- the logical and of the corresponding digits of the operands, aligned at the
-- least-significant digit. A result digit is 1 if both of the corresponding
-- operand digits are 1; otherwise it is 0.
and :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
and x@Num{} y@Num{} = case (toLogical x, toLogical y) of
  (Just lx, Just ly) -> getPrecision >>= \p ->
    let m = Prelude.min (bitLength lx) (bitLength ly)
        z = Logical { bits = bits lx .&. bits ly
                    , bitLength = maybe m (Prelude.min m) p }
    in return (fromLogical z)
  _ -> invalidOperation qNaN
and x y = generalRules2 x y

{- $doctest-and
>>> op2 Op.and "0" "0"
0

>>> op2 Op.and "0" "1"
0

>>> op2 Op.and "1" "0"
0

>>> op2 Op.and "1" "1"
1

>>> op2 Op.and "1100" "1010"
1000

>>> op2 Op.and "1111" "10"
10
-}

-- | 'or' is a logical operation which takes two logical operands. The result
-- is the digit-wise /inclusive or/ of the two operands; each digit of the
-- result is the logical or of the corresponding digits of the operands,
-- aligned at the least-significant digit. A result digit is 1 if either or
-- both of the corresponding operand digits is 1; otherwise it is 0.
or :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
or x@Num{} y@Num{} = case (toLogical x, toLogical y) of
  (Just lx, Just ly) -> getPrecision >>= \p ->
    let m = Prelude.max (bitLength lx) (bitLength ly)
        z = Logical { bits = bits lx .|. bits ly
                    , bitLength = maybe m (Prelude.min m) p }
    in return (fromLogical z)
  _ -> invalidOperation qNaN
or x y = generalRules2 x y

{- $doctest-or
>>> op2 Op.or "0" "0"
0

>>> op2 Op.or "0" "1"
1

>>> op2 Op.or "1" "0"
1

>>> op2 Op.or "1" "1"
1

>>> op2 Op.or "1100" "1010"
1110

>>> op2 Op.or "1110" "10"
1110
-}

-- | 'xor' is a logical operation which takes two logical operands. The result
-- is the digit-wise /exclusive or/ of the two operands; each digit of the
-- result is the logical exclusive-or of the corresponding digits of the
-- operands, aligned at the least-significant digit. A result digit is 1 if
-- one of the corresponding operand digits is 1 and the other is 0; otherwise
-- it is 0.
xor :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
xor x@Num{} y@Num{} = case (toLogical x, toLogical y) of
  (Just lx, Just ly) -> getPrecision >>= \p ->
    let m = Prelude.max (bitLength lx) (bitLength ly)
        z = Logical { bits = bits lx `Bits.xor` bits ly
                    , bitLength = maybe m (Prelude.min m) p }
    in return (fromLogical z)
  _ -> invalidOperation qNaN
xor x y = generalRules2 x y

{- $doctest-xor
>>> op2 Op.xor "0" "0"
0

>>> op2 Op.xor "0" "1"
1

>>> op2 Op.xor "1" "0"
1

>>> op2 Op.xor "1" "1"
0

>>> op2 Op.xor "1100" "1010"
110

>>> op2 Op.xor "1111" "10"
1101
-}

-- | 'invert' is a logical operation which takes one logical operand. The
-- result is the digit-wise /inversion/ of the operand; each digit of the
-- result is the inverse of the corresponding digit of the operand. A result
-- digit is 1 if the corresponding operand digit is 0; otherwise it is 0.
invert :: FinitePrecision p => Decimal a b -> Arith p r (Decimal p r)
invert x@Num{} = case toLogical x of
  Just lx -> getPrecision >>= \(Just p) ->
    let z = Logical { bits = complement (bits lx), bitLength = p }
    in return (fromLogical z)
  _ -> invalidOperation qNaN
invert x = generalRules1 x

{- $doctest-invert
>>> op1 Op.invert "0"
111111111

>>> op1 Op.invert "1"
111111110

>>> op1 Op.invert "111111111"
0

>>> op1 Op.invert "101010101"
10101010
-}

-- | 'canonical' takes one operand. The result has the same value as the
-- operand but always uses a /canonical/ encoding. The definition of
-- /canonical/ is implementation-defined; if more than one internal encoding
-- for a given NaN, Infinity, or finite number is possible then one
-- “preferred” encoding is deemed canonical. This operation then returns the
-- value using that preferred encoding.
--
-- If all possible operands have just one internal encoding each, then
-- 'canonical' always returns the operand unchanged (that is, it has the same
-- effect as 'copy'). This operation is unaffected by context and is quiet —
-- no /flags/ are changed in the context.
canonical :: Decimal a b -> Arith p r (Decimal a b)
canonical = copy

{- $doctest-canonical
>>> op1 Op.canonical "2.50"
2.50
-}

-- | 'class'' takes one operand. The result is an indication of the /class/ of
-- the operand, where the class is one of ten possibilities, corresponding to
-- one of the strings @"sNaN"@ (signaling NaN), @\"NaN"@ (quiet NaN),
-- @"-Infinity"@ (negative infinity), @"-Normal"@ (negative normal finite
-- number), @"-Subnormal"@ (negative subnormal finite number), @"-Zero"@
-- (negative zero), @"+Zero"@ (non-negative zero), @"+Subnormal"@ (positive
-- subnormal finite number), @"+Normal"@ (positive normal finite number), or
-- @"+Infinity"@ (positive infinity). This operation is quiet; no /flags/ are
-- changed in the context.
--
-- Note that unlike the special values in the model, the sign of any NaN is
-- ignored in the classification, as required by IEEE 754.
class' :: Precision a => Decimal a b -> Arith p r Class
class' n = return $ case n of
  Num {} | Number.isZero n      -> NumberClass (sign n) ZeroClass
         | Number.isSubnormal n -> NumberClass (sign n) SubnormalClass
         | otherwise            -> NumberClass (sign n) NormalClass
  Inf {}                        -> NumberClass (sign n) InfinityClass
  NaN { signaling = s }         -> NaNClass (toEnum . fromEnum $ s)

data Class = NumberClass Sign NumberClass -- ^ Number (finite or infinite)
           | NaNClass NaNClass            -- ^ Not a number (quiet or signaling)
           deriving Eq

data NumberClass = ZeroClass       -- ^ Zero
                 | SubnormalClass  -- ^ Subnormal finite number
                 | NormalClass     -- ^ Normal finite number
                 | InfinityClass   -- ^ Infinity
                 deriving Eq

data NaNClass = QuietClass      -- ^ Quiet NaN
              | SignalingClass  -- ^ Signaling NaN
              deriving (Eq, Enum)

instance Show Class where
  show c = case c of
    NumberClass s nc        -> signChar s : showNumberClass nc
    NaNClass QuietClass     ->       nan
    NaNClass SignalingClass -> 's' : nan

    where signChar :: Sign -> Char
          signChar Pos = '+'
          signChar Neg = '-'

          showNumberClass :: NumberClass -> String
          showNumberClass nc = case nc of
            ZeroClass      -> "Zero"
            SubnormalClass -> "Subnormal"
            NormalClass    -> "Normal"
            InfinityClass  -> "Infinity"

          nan :: String
          nan = "NaN"

{- $doctest-class'
>>> op1 Op.class' "Infinity"
+Infinity

>>> op1 Op.class' "1E-10"
+Normal

>>> op1 Op.class' "2.50"
+Normal

>>> op1 Op.class' "0.1E-999"
+Subnormal

>>> op1 Op.class' "0"
+Zero

>>> op1 Op.class' "-0"
-Zero

>>> op1 Op.class' "-0.1E-999"
-Subnormal

>>> op1 Op.class' "-1E-10"
-Normal

>>> op1 Op.class' "-2.50"
-Normal

>>> op1 Op.class' "-Infinity"
-Infinity

>>> op1 Op.class' "NaN"
NaN

>>> op1 Op.class' "-NaN"
NaN

>>> op1 Op.class' "sNaN"
sNaN
-}

-- | 'compareTotal' takes two operands and compares them using their abstract
-- representation rather than their numerical value. A /total ordering/ is
-- defined for all possible abstract representations, as described below. If
-- the first operand is lower in the total order than the second operand then
-- the result is 'LT', if the operands have the same abstract representation
-- then the result is 'EQ', and if the first operand is higher in the total
-- order than the second operand then the result is 'GT'. The total ordering
-- is defined as follows.
--
-- 1. The following items describe the ordering for representations whose
-- /sign/ is 0. If the /sign/ is 1, the order is reversed. A representation
-- with a /sign/ of 1 is always lower in the ordering than one with a /sign/
-- of 0.
--
-- 2. Numbers (representations which are not NaNs) are ordered such that a
-- larger numerical value is higher in the ordering. If two representations
-- have the same numerical value then the exponent is taken into account;
-- larger (more positive) exponents are higher in the ordering.
--
-- 3. All quiet NaNs are higher in the total ordering than all signaling NaNs.
--
-- 4. Quiet NaNs and signaling NaNs are ordered according to their /payload/;
-- a larger payload is higher in the ordering.
--
-- For example, the following values are ordered from lowest to highest: @-NaN
-- -sNaN -Infinity -127 -1 -1.00 -0 -0.000 0 1.2300 1.23 1E+9 Infinity sNaN
-- NaN NaN456@.
compareTotal :: Decimal a b -> Decimal c d -> Arith p r Ordering
compareTotal x y = return $ case (sign x, sign y) of
  (Pos, Pos) -> compareAbs x y
  (Neg, Neg) -> compareAbs y x
  (Neg, Pos) -> LT
  (Pos, Neg) -> GT

  where compareAbs :: Decimal a b -> Decimal c d -> Ordering
        compareAbs Num { coefficient = xc, exponent = xe }
                   Num { coefficient = yc, exponent = ye } =
          let (xac, yac) | xe == ye  = (xc, yc)
                         | xe >  ye  = (xc * 10^n, yc)
                         | otherwise = (xc, yc * 10^n)
              n = Prelude.abs (xe - ye)
          in Prelude.compare xac yac `mappend` Prelude.compare xe ye
        compareAbs Num{} Inf{} = LT
        compareAbs Inf{} Num{} = GT
        compareAbs Inf{} Inf{} = EQ
        compareAbs NaN { signaling = xs, payload = xp }
                   NaN { signaling = ys, payload = yp } =
          Prelude.compare ys xs `mappend` Prelude.compare xp yp
        compareAbs NaN{} _     = GT
        compareAbs _     NaN{} = LT

{- $doctest-compareTotal
>>> fromOrdering $ op2 Op.compareTotal "12.73" "127.9"
-1

>>> fromOrdering $ op2 Op.compareTotal "-127" "12"
-1

>>> fromOrdering $ op2 Op.compareTotal "12.30" "12.3"
-1

>>> fromOrdering $ op2 Op.compareTotal "12.30" "12.30"
0

>>> fromOrdering $ op2 Op.compareTotal "12.3" "12.300"
1

>>> fromOrdering $ op2 Op.compareTotal "12.3" "NaN"
-1
-}

-- | 'compareTotalMagnitude' takes two operands and compares them using their
-- abstract representation rather than their numerical value and with their
-- /sign/ ignored and assumed to be 0. The result is identical to that
-- obtained by using 'compareTotal' on two operands which are the 'copyAbs'
-- copies of the operands to 'compareTotalMagnitude'.
compareTotalMagnitude :: Decimal a b -> Decimal c d -> Arith p r Ordering
compareTotalMagnitude x y = compareTotal x { sign = Pos } y { sign = Pos }

-- | 'copy' takes one operand. The result is a copy of the operand. This
-- operation is unaffected by context and is quiet — no /flags/ are changed in
-- the context.
copy :: Decimal a b -> Arith p r (Decimal a b)
copy = return

{- $doctest-copy
>>> op1 Op.copy "2.1"
2.1

>>> op1 Op.copy "-1.00"
-1.00
-}

-- | 'copyAbs' takes one operand. The result is a copy of the operand with the
-- /sign/ set to 0. Unlike the 'abs' operation, this operation is unaffected
-- by context and is quiet — no /flags/ are changed in the context.
copyAbs :: Decimal a b -> Arith p r (Decimal a b)
copyAbs n = return n { sign = Pos }

{- $doctest-copyAbs
>>> op1 Op.copyAbs "2.1"
2.1

>>> op1 Op.copyAbs "-100"
100
-}

-- | 'copyNegate' takes one operand. The result is a copy of the operand with
-- the /sign/ inverted (a /sign/ of 0 becomes 1 and vice versa). Unlike the
-- 'minus' operation, this operation is unaffected by context and is quiet —
-- no /flags/ are changed in the context.
copyNegate :: Decimal a b -> Arith p r (Decimal a b)
copyNegate n = return n { sign = negateSign (sign n) }

{- $doctest-copyNegate
>>> op1 Op.copyNegate "101.5"
-101.5

>>> op1 Op.copyNegate "-101.5"
101.5
-}

-- | 'copySign' takes two operands. The result is a copy of the first operand
-- with the /sign/ set to be the same as the /sign/ of the second
-- operand. This operation is unaffected by context and is quiet — no /flags/
-- are changed in the context.
copySign :: Decimal a b -> Decimal c d -> Arith p r (Decimal a b)
copySign n m = return n { sign = sign m }

{- $doctest-copySign
>>> op2 Op.copySign  "1.50"  "7.33"
1.50

>>> op2 Op.copySign "-1.50"  "7.33"
1.50

>>> op2 Op.copySign  "1.50" "-7.33"
-1.50

>>> op2 Op.copySign "-1.50" "-7.33"
-1.50
-}

-- | 'isCanonical' takes one operand. The result is 'True' if the operand is
-- /canonical/; otherwise it is 'False'. The definition of /canonical/ is
-- implementation-defined; if more than one internal encoding for a given NaN,
-- Infinity, or finite number is possible then one “preferred” encoding is
-- deemed canonical. This operation then tests whether the internal encoding
-- is that preferred encoding.
--
-- If all possible operands have just one internal encoding each, then
-- 'isCanonical' always returns 'True'. This operation is unaffected by
-- context and is quiet — no /flags/ are changed in the context.
isCanonical :: Decimal a b -> Arith p r Bool
isCanonical _ = return True

{- $doctest-isCanonical
>>> fromBool $ op1 Op.isCanonical "2.50"
1
-}

-- | 'isFinite' takes one operand. The result is 'True' if the operand is
-- neither infinite nor a NaN (that is, it is a normal number, a subnormal
-- number, or a zero); otherwise it is 'False'. This operation is unaffected
-- by context and is quiet — no /flags/ are changed in the context.
isFinite :: Decimal a b -> Arith p r Bool
isFinite = return . Number.isFinite

{- $doctest-isFinite
>>> fromBool $ op1 Op.isFinite "2.50"
1

>>> fromBool $ op1 Op.isFinite "-0.3"
1

>>> fromBool $ op1 Op.isFinite "0"
1

>>> fromBool $ op1 Op.isFinite "Inf"
0

>>> fromBool $ op1 Op.isFinite "NaN"
0
-}

-- | 'isInfinite' takes one operand. The result is 'True' if the operand is an
-- Infinity; otherwise it is 'False'. This operation is unaffected by context
-- and is quiet — no /flags/ are changed in the context.
isInfinite :: Decimal a b -> Arith p r Bool
isInfinite n = return $ case n of
  Inf{} -> True
  _     -> False

{- $doctest-isInfinite
>>> fromBool $ op1 Op.isInfinite "2.50"
0

>>> fromBool $ op1 Op.isInfinite "-Inf"
1

>>> fromBool $ op1 Op.isInfinite "NaN"
0
-}

-- | 'isNaN' takes one operand. The result is 'True' if the operand is a NaN
-- (quiet or signaling); otherwise it is 'False'. This operation is unaffected
-- by context and is quiet — no /flags/ are changed in the context.
isNaN :: Decimal a b -> Arith p r Bool
isNaN n = return $ case n of
  NaN{} -> True
  _     -> False

{- $doctest-isNaN
>>> fromBool $ op1 Op.isNaN "2.50"
0

>>> fromBool $ op1 Op.isNaN "NaN"
1

>>> fromBool $ op1 Op.isNaN "-sNaN"
1
-}

-- | 'isNormal' takes one operand. The result is 'True' if the operand is a
-- positive or negative /normal number/; otherwise it is 'False'. This
-- operation is quiet; no /flags/ are changed in the context.
isNormal :: Precision a => Decimal a b -> Arith p r Bool
isNormal = return . Number.isNormal

{- $doctest-isNormal
>>> fromBool $ op1 Op.isNormal "2.50"
1

>>> fromBool $ op1 Op.isNormal "0.1E-999"
0

>>> fromBool $ op1 Op.isNormal "0.00"
0

>>> fromBool $ op1 Op.isNormal "-Inf"
0

>>> fromBool $ op1 Op.isNormal "NaN"
0
-}

-- | 'isQNaN' takes one operand. The result is 'True' if the operand is a
-- quiet NaN; otherwise it is 'False'. This operation is unaffected by context
-- and is quiet — no /flags/ are changed in the context.
isQNaN :: Decimal a b -> Arith p r Bool
isQNaN n = return $ case n of
  NaN { signaling = False } -> True
  _                         -> False

{- $doctest-isQNaN
>>> fromBool $ op1 Op.isQNaN "2.50"
0

>>> fromBool $ op1 Op.isQNaN "NaN"
1

>>> fromBool $ op1 Op.isQNaN "sNaN"
0
-}

-- | 'isSigned' takes one operand. The result is 'True' if the /sign/ of the
-- operand is 1; otherwise it is 'False'. This operation is unaffected by
-- context and is quiet — no /flags/ are changed in the context.
isSigned :: Decimal a b -> Arith p r Bool
isSigned = return . Number.isNegative

{- $doctest-isSigned
>>> fromBool $ op1 Op.isSigned "2.50"
0

>>> fromBool $ op1 Op.isSigned "-12"
1

>>> fromBool $ op1 Op.isSigned "-0"
1
-}

-- | 'isSNaN' takes one operand. The result is 'True' if the operand is a
-- signaling NaN; otherwise it is 'False'. This operation is unaffected by
-- context and is quiet — no /flags/ are changed in the context.
isSNaN :: Decimal a b -> Arith p r Bool
isSNaN n = return $ case n of
  NaN { signaling = True } -> True
  _                        -> False

{- $doctest-isSNaN
>>> fromBool $ op1 Op.isSNaN "2.50"
0

>>> fromBool $ op1 Op.isSNaN "NaN"
0

>>> fromBool $ op1 Op.isSNaN "sNaN"
1
-}

-- | 'isSubnormal' takes one operand. The result is 'True' if the operand is a
-- positive or negative /subnormal number/; otherwise it is 'False'. This
-- operation is quiet; no /flags/ are changed in the context.
isSubnormal :: Precision a => Decimal a b -> Arith p r Bool
isSubnormal = return . Number.isSubnormal

{- $doctest-isSubnormal
>>> fromBool $ op1 Op.isSubnormal "2.50"
0

>>> fromBool $ op1 Op.isSubnormal "0.1E-999"
1

>>> fromBool $ op1 Op.isSubnormal "0.00"
0

>>> fromBool $ op1 Op.isSubnormal "-Inf"
0

>>> fromBool $ op1 Op.isSubnormal "NaN"
0
-}

-- | 'isZero' takes one operand. The result is 'True' if the operand is a
-- zero; otherwise it is 'False'. This operation is unaffected by context and
-- is quiet — no /flags/ are changed in the context.
isZero :: Decimal a b -> Arith p r Bool
isZero = return . Number.isZero

{- $doctest-isZero
>>> fromBool $ op1 Op.isZero "0"
1

>>> fromBool $ op1 Op.isZero "2.50"
0

>>> fromBool $ op1 Op.isZero "-0E+2"
1
-}

-- | 'logb' takes one operand. If the operand is a NaN then the general
-- arithmetic rules apply. If the operand is infinite then +Infinity is
-- returned. If the operand is a zero, then −Infinity is returned and the
-- Division by zero exceptional condition is raised.
--
-- Otherwise, the result is the integer which is the exponent of the magnitude
-- of the most significant digit of the operand (as though the operand were
-- truncated to a single digit while maintaining the value of that digit and
-- without limiting the resulting exponent). All results are exact unless an
-- integer result does not fit in the available /precision/.
logb :: (Precision p, Rounding r) => Decimal a b -> Arith p r (Decimal p r)
logb Num { coefficient = c, exponent = e }
  | c == 0    = raiseSignal DivisionByZero Inf { sign = Neg }
  | otherwise = roundDecimal (fromInteger r :: Decimal PInfinite RoundHalfEven)
  where r = fromIntegral (numDigits c) - 1 + fromIntegral e :: Integer
logb Inf{} = return Inf { sign = Pos }
logb x = generalRules1 x

{- $doctest-logb
>>> op1 Op.logb "250"
2

>>> op1 Op.logb "2.50"
0

>>> op1 Op.logb "0.03"
-2

>>> op1 Op.logb "0"
-Infinity
-}

-- | 'scaleb' takes two operands. If either operand is a NaN then the general
-- arithmetic rules apply. Otherwise, the second operand must be a finite
-- integer with an exponent of zero and in the range ±2 × (E/max/ +
-- /precision/) inclusive, where E/max/ is the largest value that can be
-- returned by the 'logb' operation at the same /precision/ setting. (If is is
-- not, the Invalid Operation condition is raised and the result is NaN.)
--
-- If the first operand is infinite then that Infinity is returned, otherwise
-- the result is the first operand modified by adding the value of the second
-- operand to its /exponent/. The result may Overflow or Underflow.
scaleb :: Decimal a b -> Decimal c d -> Arith p r (Decimal a b)
scaleb x@Num { exponent = e } s
  | validScale s = let Just i = integralValue s
                   in return x { exponent = e + fromInteger i }
                      -- XXX check for Overflow and Underflow
scaleb x@Inf{} s | validScale s = return x
scaleb x y = coerce <$> generalRules2 x y

validScale :: Decimal a b -> Bool
validScale Num { exponent = 0 } = True  -- XXX
validScale _                    = False

{- $doctest-scaleb
>>> op2 Op.scaleb "7.50" "-2"
0.0750

>>> op2 Op.scaleb "7.50" "0"
7.50

>>> op2 Op.scaleb "7.50" "3"
7.50E+3
-}

-- | 'radix' takes no operands. The result is the radix (base) in which
-- arithmetic is effected; for this specification the result will have the
-- value 10.
radix :: Precision p => Arith p r (Decimal p r)
radix = return radix'
  where radix' = case precision radix' of
          Just 1 -> one { exponent    =  1 }
          _      -> one { coefficient = 10 }

{- $doctest-radix
>>> op0 Op.radix
10
-}

-- | 'sameQuantum' takes two operands, and returns 'True' if the two operands
-- have the same /exponent/ or 'False' otherwise. The result is never affected
-- by either the sign or the coefficient of either operand.
--
-- If either operand is a /special value/, 'True' is returned only if both
-- operands are NaNs or both are infinities.
--
-- 'sameQuantum' does not change any /flags/ in the context.
sameQuantum :: Decimal a b -> Decimal c d -> Arith p r Bool
sameQuantum Num { exponent = xe } Num { exponent = ye } = return (xe == ye)
sameQuantum Inf {               } Inf {               } = return True
sameQuantum NaN {               } NaN {               } = return True
sameQuantum _                     _                     = return False

{- $doctest-sameQuantum
>>> fromBool $ op2 Op.sameQuantum "2.17" "0.001"
0

>>> fromBool $ op2 Op.sameQuantum "2.17" "0.01"
1

>>> fromBool $ op2 Op.sameQuantum "2.17" "0.1"
0

>>> fromBool $ op2 Op.sameQuantum "2.17" "1"
0

>>> fromBool $ op2 Op.sameQuantum "Inf" "-Inf"
1

>>> fromBool $ op2 Op.sameQuantum "NaN" "NaN"
1
-}

-- | 'shift' takes two operands. The second operand must be an integer (with
-- an /exponent/ of 0) in the range /−precision/ through /precision/. If the
-- first operand is a NaN then the general arithmetic rules apply, and if it
-- is infinite then the result is the Infinity unchanged.
--
-- Otherwise (the first operand is finite) the result has the same /sign/ and
-- /exponent/ as the first operand, and a /coefficient/ which is a shifted
-- copy of the digits in the coefficient of the first operand. The number of
-- places to shift is taken from the absolute value of the second operand,
-- with the shift being to the left if the second operand is positive or to
-- the right otherwise. Digits shifted into the coefficient are zeros.
--
-- The only /flag/ that might be set is 'InvalidOperation' (set if the first
-- operand is an sNaN or the second is not valid).
--
-- The 'rotate' operation can be used to rotate rather than shift a
-- coefficient.
shift :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
shift n@Num { coefficient = c } s@Num { sign = d, coefficient = sc }
  | validShift z s = return z
  where z = case precision z of
          Just p  -> y { coefficient = coefficient y `rem` 10 ^ p }
          Nothing -> y
        y = case d of
          Pos -> n { coefficient =  c  *     10 ^ sc }
          Neg -> n { coefficient =  c `quot` 10 ^ sc }

shift n@Inf {                   } s | validShift z s = return z
  where z = coerce n
shift n@NaN { signaling = False } s | validShift z s = return z
  where z = coerce n
shift n@NaN { signaling = True  } _                  = invalidOperation n
shift _                           s                  = invalidOperation s

validShift :: Precision p => p -> Decimal a b -> Bool
validShift px Num { coefficient = c, exponent = 0 } =
  let p = fromIntegral <$> precision px in maybe True (c <=) p
validShift _ _ = False

{- $doctest-shift
>>> op2 Op.shift "34" "8"
400000000

>>> op2 Op.shift "12" "9"
0

>>> op2 Op.shift "123456789" "-2"
1234567

>>> op2 Op.shift "123456789" "0"
123456789

>>> op2 Op.shift "123456789" "+2"
345678900
-}

-- | 'rotate' takes two operands. The second operand must be an integer (with
-- an /exponent/ of 0) in the range /−precision/ through /precision/. If the
-- first operand is a NaN then the general arithmetic rules apply, and if it
-- is infinite then the result is the Infinity unchanged.
--
-- Otherwise (the first operand is finite) the result has the same /sign/ and
-- /exponent/ as the first operand, and a /coefficient/ which is a rotated
-- copy of the digits in the coefficient of the first operand. The number of
-- places of rotation is taken from the absolute value of the second operand,
-- with the rotation being to the left if the second operand is positive or to
-- the right otherwise.
--
-- If the coefficient of the first operand has fewer than /precision/ digits,
-- it is treated as though it were padded on the left with zeros to length
-- /precision/ before the rotation. Similarly, if the coefficient of the first
-- operand has more than /precision/ digits, it is truncated on the left
-- before use.
--
-- The only /flag/ that might be set is 'InvalidOperation' (set if the first
-- operand is an sNaN or the second is not valid).
--
-- The 'shift' operation can be used to shift rather than rotate a
-- coefficient.
rotate :: FinitePrecision p
       => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
rotate n@Num { coefficient = c } s@Num { sign = d, coefficient = sc }
  | validShift z s = return z
  where z = n { coefficient = rc * b + (lc `rem` b) }
        (lc, rc) = c `quotRem` b'
        (b , b') = case d of
          Pos -> (10^sc , 10^sc')
          Neg -> (10^sc', 10^sc )
        sc' = finitePrecision z - fromIntegral sc

rotate n@Inf {                   } s | validShift z s = return z
  where z = coerce n
rotate n@NaN { signaling = False } s | validShift z s = return z
  where z = coerce n
rotate n@NaN { signaling = True  } _                  = invalidOperation n
rotate _                           s                  = invalidOperation s

{- $doctest-rotate
>>> op2 Op.rotate "34" "8"
400000003

>>> op2 Op.rotate "12" "9"
12

>>> op2 Op.rotate "123456789" "-2"
891234567

>>> op2 Op.rotate "123456789" "0"
123456789

>>> op2 Op.rotate "123456789" "+2"
345678912
-}
