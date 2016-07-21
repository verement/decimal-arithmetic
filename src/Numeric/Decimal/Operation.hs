
{- | Eventually most or all of the arithmetic operations described in the
/General Decimal Arithmetic Specification/ will be provided here. For now, the
operations are mostly limited to those exposed through various class methods.

It is suggested to import this module qualified to avoid "Prelude" name
clashes:

> import qualified Numeric.Decimal.Operation as Op

Note that it is not usually necessary to import this module unless you want to
use operations unavailable through class methods, or you need precise control
over the handling of exceptional conditions.
-}
module Numeric.Decimal.Operation
       ( -- * Arithmetic operations
         -- $arithmetic-operations

         abs
       , add
       , subtract
       , compare
         -- compareSignal
       , divide
         -- divideInteger
         -- exp
         -- fusedMultiplyAdd
         -- ln
         -- log10
       , max
       , maxMagnitude
       , min
       , minMagnitude
       , minus
       , plus
       , multiply
         -- nextMinus
         -- nextPlus
         -- nextToward
         -- power
         -- quantize
         , reduce
         -- remainder
         -- remainderNear
         -- roundToIntegralExact
         -- roundToIntegralValue
         -- squareRoot

         -- * Miscellaneous operations
         -- $miscellaneous-operations

         -- and
         , canonical
         , class_, Class(..), Sign(..), Subclass(..)
         -- compareTotal
         -- compareTotalMagnitude
         , copy
         , copyAbs
         , copyNegate
         , copySign
         -- invert
         , isCanonical
         , isFinite
         , isInfinite
         , isNaN
         , isNormal
         , isQNaN
         , isSigned
         , isSNaN
         , isSubnormal
         , isZero
         -- logb
         -- or
         , radix
         -- rotate
         , sameQuantum
         -- scaleb
         -- shift
         -- xor
       ) where

import Prelude hiding (abs, compare, exponent, isInfinite, isNaN, max, min,
                       round, subtract)
import qualified Prelude

import Data.Coerce (coerce)

import Numeric.Decimal.Arithmetic
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
result = round  -- ...
--  | maybe False (numDigits c >) (precision r) = undefined

invalidOperation :: Decimal a b -> Arith p r (Decimal p r)
invalidOperation n = raiseSignal InvalidOperation qNaN

toQNaN :: Decimal a b -> Decimal p r
toQNaN SNaN { sign = s, payload = p } = QNaN { sign = s, payload = p }
toQNaN n@QNaN{}                       = coerce n
toQNaN n                              = qNaN { sign = sign n }

toQNaN2 :: Decimal a b -> Decimal c d -> Decimal p r
toQNaN2 nan@SNaN{} _ = toQNaN nan
toQNaN2 _ nan@SNaN{} = toQNaN nan
toQNaN2 nan@QNaN{} _ = coerce nan
toQNaN2 _ nan@QNaN{} = coerce nan
toQNaN2 n _          = toQNaN n

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
  | otherwise = invalidOperation inf
add inf@Inf{} Num{} = return (coerce inf)
add Num{} inf@Inf{} = return (coerce inf)
add x y             = return (toQNaN2 x y)

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
multiply Inf { sign = xs } Num { sign = ys } =
  return Inf { sign = xorSigns xs ys }
multiply Num { sign = xs } Inf { sign = ys } =
  return Inf { sign = xorSigns xs ys }
multiply x y = return (toQNaN2 x y)

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

-- | 'divide' takes two operands. If either operand is a /special value/ then the general rules apply.
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
  | Number.isZero dividend = invalidOperation qNaN
  | otherwise              = raiseSignal DivisionByZero
                        infinity { sign = xorSigns xs ys }
divide Num { sign = xs, coefficient = xc, exponent = xe }
       Num { sign = ys, coefficient = yc, exponent = ye } = quotient

  where quotient = result =<< answer
        rn = Num { sign = rs, coefficient = rc, exponent = re }
        rs = xorSigns xs ys
        (rc, rem, dv, adjust) = longDivision xc yc (finitePrecision rn)
        re = xe - (ye + adjust)
        answer
          | rem == 0  = return rn
          | otherwise = round $ case (rem * 2) `Prelude.compare` dv of
              LT -> rn { coefficient = rc * 10 + 1, exponent = re - 1 }
              EQ -> rn { coefficient = rc * 10 + 5, exponent = re - 1 }
              GT -> rn { coefficient = rc * 10 + 9, exponent = re - 1 }

divide Inf{} Inf{} = invalidOperation qNaN
divide Inf { sign = xs } Num { sign = ys } =
  return Inf { sign = xorSigns xs ys }
divide Num { sign = xs } Inf { sign = ys } =
  return zero { sign = xorSigns xs ys }
divide x y = return (toQNaN2 x y)

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
-- Otherwise, the operands are compared, returning @-1@ if the first is less
-- than the second, @0@ if they are equal, or @1@ if the first is greater than
-- the second.
compare :: (Precision p, Rounding r)
        => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
compare x@Num{} y@Num{} = nzp <$> (xn `subtract` yn)

  where (xn, yn) | sign x /= sign y = (nzp x, nzp y)
                 | otherwise        = (x, y)

        nzp :: Decimal p r -> Decimal p r
        nzp Num { sign = s, coefficient = c }
          | c == 0    = zero
          | s == Pos  = one
          | otherwise = negativeOne
        nzp Inf { sign = s }
          | s == Pos  = one
          | otherwise = negativeOne
        nzp n = toQNaN n

compare Inf { sign = xs } Inf { sign = ys }
  | xs == ys  = return zero
  | xs == Neg = return negativeOne
  | otherwise = return one
compare Inf { sign = xs } Num { }
  | xs == Neg = return negativeOne
  | otherwise = return one
compare Num { } Inf { sign = ys }
  | ys == Pos = return negativeOne
  | otherwise = return one
compare nan@SNaN{} _ = invalidOperation nan
compare _ nan@SNaN{} = invalidOperation nan
compare x y          = return (toQNaN2 x y)

{- $doctest-compare
>>> op2 Op.compare "2.1" "3"
-1

>>> op2 Op.compare "2.1" "2.1"
0

>>> op2 Op.compare "2.1" "2.10"
0

>>> op2 Op.compare "3" "2.1"
1

>>> op2 Op.compare "2.1" "-3"
1

>>> op2 Op.compare "-3" "2.1"
-1
-}

-- | 'max' takes two operands, compares their values numerically, and returns
-- the maximum. If either operand is a NaN then the general rules apply,
-- unless one is a quiet NaN and the other is numeric, in which case the
-- numeric operand is returned.
max :: (Precision p, Rounding r)
    => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
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
maxMagnitude :: (Precision p, Rounding r)
             => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
maxMagnitude x y = snd <$> minMax withoutSign x y

-- | 'min' takes two operands, compares their values numerically, and returns
-- the minimum. If either operand is a NaN then the general rules apply,
-- unless one is a quiet NaN and the other is numeric, in which case the
-- numeric operand is returned.
min :: (Precision p, Rounding r)
    => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
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
minMagnitude :: (Precision p, Rounding r)
             => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
minMagnitude x y = fst <$> minMax withoutSign x y

-- | Ordering function for 'min', 'minMagnitude', 'max', and 'maxMagnitude':
-- returns the original arguments as (smaller, larger) when the given function
-- is applied to them.
minMax :: (Precision p, Rounding r)
       => (Decimal a b -> Decimal a b) -> Decimal a b -> Decimal a b
       -> Arith p r (Decimal a b, Decimal a b)
minMax _ x@Num{}  QNaN{} = return (x, x)
minMax _ x@Inf{}  QNaN{} = return (x, x)
minMax _  QNaN{} y@Num{} = return (y, y)
minMax _  QNaN{} y@Inf{} = return (y, y)

minMax f x y = do
  c <- f x `compare` f y
  return $ case c of
    Num { coefficient = 0 } -> case (sign x, sign y) of
      (Neg, Pos) -> (x, y)
      (Pos, Neg) -> (y, x)
      (Pos, Pos) -> case (x, y) of
        (Num { exponent = xe }, Num { exponent = ye }) | xe > ye -> (y, x)
        _ -> (x, y)
      (Neg, Neg) -> case (x, y) of
        (Num { exponent = xe }, Num { exponent = ye }) | xe < ye -> (y, x)
        _ -> (x, y)
    Num { sign = Pos } -> (y, x)
    Num { sign = Neg } -> (x, y)
    nan -> let nan' = coerce nan in (nan', nan')


withoutSign :: Decimal p r -> Decimal p r
withoutSign n = n { sign = Pos }

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

-- $miscellaneous-operations
--
-- This section describes miscellaneous operations on decimal numbers,
-- including non-numeric comparisons, sign and other manipulations, and
-- logical operations.

-- | 'canonical' takes one operand. The result has the same value as the
-- operand but always uses a /canonical/ encoding. The definition of
-- /canonical/ is implementation-defined; if more than one internal encoding
-- for a given NaN, Infinity, or finite number is possible then one
-- “preferred” encoding is deemed canonical. This operation then returns the
-- value using that preferred encoding.
--
-- If all possible operands have just one internal encoding each, then
-- 'canonical' always returns the operand unchanged (that is, it has the same
-- effect as 'copy'). This operation is unaffected by context and is quiet –
-- no /flags/ are changed in the context.
canonical :: Decimal a b -> Arith p r (Decimal a b)
canonical = return

{- $doctest-canonical
>>> op1 Op.canonical "2.50"
2.50
-}

-- | 'class_' takes one operand. The result is an indication of the /class/ of
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
class_ :: Precision a => Decimal a b -> Arith p r Class
class_ n = return $ case n of
  Num {} | Number.isZero n      -> Class (sign n) ZeroClass
         | Number.isSubnormal n -> Class (sign n) SubnormalClass
         | otherwise            -> Class (sign n) NormalClass
  Inf {}                        -> Class (sign n) InfinityClass
  QNaN{}                        -> Class  Pos     NaNClass
  SNaN{}                        -> Class  Neg     NaNClass

data Class = Class Sign Subclass deriving Eq

data Subclass = ZeroClass       -- ^ Zero
              | NormalClass     -- ^ Normal finite number
              | SubnormalClass  -- ^ Subnormal finite number
              | InfinityClass   -- ^ Infinity
              | NaNClass        -- ^ Not a number (quiet or signaling)
              deriving Eq

instance Show Class where
  show c = case c of
    Class Pos s@NaNClass ->       showSubclass s
    Class Neg s@NaNClass -> 's' : showSubclass s
    Class Pos s          -> '+' : showSubclass s
    Class Neg s          -> '-' : showSubclass s

    where showSubclass s = case s of
            ZeroClass      -> "Zero"
            NormalClass    -> "Normal"
            SubnormalClass -> "Subnormal"
            InfinityClass  -> "Infinity"
            NaNClass       -> "NaN"

{- $doctest-class_
>>> op1 Op.class_ "Infinity"
+Infinity

>>> op1 Op.class_ "1E-10"
+Normal

>>> op1 Op.class_ "2.50"
+Normal

>>> op1 Op.class_ "0.1E-999"
+Subnormal

>>> op1 Op.class_ "0"
+Zero

>>> op1 Op.class_ "-0"
-Zero

>>> op1 Op.class_ "-0.1E-999"
-Subnormal

>>> op1 Op.class_ "-1E-10"
-Normal

>>> op1 Op.class_ "-2.50"
-Normal

>>> op1 Op.class_ "-Infinity"
-Infinity

>>> op1 Op.class_ "NaN"
NaN

>>> op1 Op.class_ "-NaN"
NaN

>>> op1 Op.class_ "sNaN"
sNaN
-}

-- | 'copy' takes one operand. The result is a copy of the operand. This
-- operation is unaffected by context and is quiet – no /flags/ are changed in
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
-- by context and is quiet – no /flags/ are changed in the context.
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
-- 'minus' operation, this operation is unaffected by context and is quiet –
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
-- operand. This operation is unaffected by context and is quiet – no /flags/
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

-- | 'isCanonical' takes one operand. The result is 1 if the operand is
-- /canonical/; otherwise it is 0. The definition of /canonical/ is
-- implementation-defined; if more than one internal encoding for a given NaN,
-- Infinity, or finite number is possible then one “preferred” encoding is
-- deemed canonical. This operation then tests whether the internal encoding
-- is that preferred encoding.
--
-- If all possible operands have just one internal encoding each, then
-- 'isCanonical' always returns 1. This operation is unaffected by context and
-- is quiet – no /flags/ are changed in the context.
isCanonical :: Decimal a b -> Arith p r (Decimal p r)
isCanonical _ = return one

{- $doctest-isCanonical
>>> op1 Op.isCanonical "2.50"
1
-}

-- | 'isFinite' takes one operand. The result is 1 if the operand is neither
-- infinite nor a NaN (that is, it is a normal number, a subnormal number, or
-- a zero); otherwise it is 0. This operation is unaffected by context and is
-- quiet – no /flags/ are changed in the context.
isFinite :: Decimal a b -> Arith p r (Decimal p r)
isFinite n = return $ case n of
  Num{} -> one
  _     -> zero

{- $doctest-isFinite
>>> op1 Op.isFinite "2.50"
1

>>> op1 Op.isFinite "-0.3"
1

>>> op1 Op.isFinite "0"
1

>>> op1 Op.isFinite "Inf"
0

>>> op1 Op.isFinite "NaN"
0
-}

-- | 'isInfinite' takes one operand. The result is 1 if the operand is an
-- Infinity; otherwise it is 0. This operation is unaffected by context and is
-- quiet – no /flags/ are changed in the context.
isInfinite :: Decimal a b -> Arith p r (Decimal p r)
isInfinite n = return $ case n of
  Inf{} -> one
  _     -> zero

{- $doctest-isInfinite
>>> op1 Op.isInfinite "2.50"
0

>>> op1 Op.isInfinite "-Inf"
1

>>> op1 Op.isInfinite "NaN"
0
-}

-- | 'isNaN' takes one operand. The result is 1 if the operand is a NaN (quiet
-- or signaling); otherwise it is 0. This operation is unaffected by context
-- and is quiet – no /flags/ are changed in the context.
isNaN :: Decimal a b -> Arith p r (Decimal p r)
isNaN n = return $ case n of
  QNaN{} -> one
  SNaN{} -> one
  _      -> zero

{- $doctest-isNaN
>>> op1 Op.isNaN "2.50"
0

>>> op1 Op.isNaN "NaN"
1

>>> op1 Op.isNaN "-sNaN"
1
-}

-- | 'isNormal' takes one operand. The result is 1 if the operand is a
-- positive or negative /normal number/; otherwise it is 0. This operation is
-- quiet; no /flags/ are changed in the context.
isNormal :: Precision a => Decimal a b -> Arith p r (Decimal p r)
isNormal n = return $ case n of
  _ | Number.isNormal n -> one
    | otherwise         -> zero

{- $doctest-isNormal
>>> op1 Op.isNormal "2.50"
1

>>> op1 Op.isNormal "0.1E-999"
0

>>> op1 Op.isNormal "0.00"
0

>>> op1 Op.isNormal "-Inf"
0

>>> op1 Op.isNormal "NaN"
0
-}

-- | 'isQNaN' takes one operand. The result is 1 if the operand is a quiet
-- NaN; otherwise it is 0. This operation is unaffected by context and is
-- quiet – no /flags/ are changed in the context.
isQNaN :: Decimal a b -> Arith p r (Decimal p r)
isQNaN n = return $ case n of
  QNaN{} -> one
  _      -> zero

{- $doctest-isQNaN
>>> op1 Op.isQNaN "2.50"
0

>>> op1 Op.isQNaN "NaN"
1

>>> op1 Op.isQNaN "sNaN"
0
-}

-- | 'isSigned' takes one operand. The result is 1 if the /sign/ of the
-- operand is 1; otherwise it is 0. This operation is unaffected by context
-- and is quiet – no /flags/ are changed in the context.
isSigned :: Decimal a b -> Arith p r (Decimal p r)
isSigned n = return $ case sign n of
  Neg -> one
  Pos -> zero

{- $doctest-isSigned
>>> op1 Op.isSigned "2.50"
0

>>> op1 Op.isSigned "-12"
1

>>> op1 Op.isSigned "-0"
1
-}

-- | 'isSNaN' takes one operand. The result is 1 if the operand is a signaling
-- NaN; otherwise it is 0. This operation is unaffected by context and is
-- quiet – no /flags/ are changed in the context.
isSNaN :: Decimal a b -> Arith p r (Decimal p r)
isSNaN n = return $ case n of
  SNaN{} -> one
  _      -> zero

{- $doctest-isSNaN
>>> op1 Op.isSNaN "2.50"
0

>>> op1 Op.isSNaN "NaN"
0

>>> op1 Op.isSNaN "sNaN"
1
-}

-- | 'isSubnormal' takes one operand. The result is 1 if the operand is a
-- positive or negative /subnormal number/; otherwise it is 0. This operation
-- is quiet; no /flags/ are changed in the context.
isSubnormal :: Precision a => Decimal a b -> Arith p r (Decimal p r)
isSubnormal n = return $ case n of
  _ | Number.isSubnormal n -> one
    | otherwise            -> zero

{- $doctest-isSubnormal
>>> op1 Op.isSubnormal "2.50"
0

>>> op1 Op.isSubnormal "0.1E-999"
1

>>> op1 Op.isSubnormal "0.00"
0

>>> op1 Op.isSubnormal "-Inf"
0

>>> op1 Op.isSubnormal "NaN"
0
-}

-- | 'isZero' takes one operand. The result is 1 if the operand is a zero;
-- otherwise it is 0. This operation is unaffected by context and is quiet –
-- no /flags/ are changed in the context.
isZero :: Decimal a b -> Arith p r (Decimal p r)
isZero n = return $ case n of
  _ | Number.isZero n -> one
    | otherwise       -> zero

{- $doctest-isZero
>>> op1 Op.isZero "0"
1

>>> op1 Op.isZero "2.50"
0

>>> op1 Op.isZero "-0E+2"
1
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

-- | 'sameQuantum' takes two operands, and returns 1 if the two operands have
-- the same /exponent/ or 0 otherwise. The result is never affected by either
-- the sign or the coefficient of either operand.
--
-- If either operand is a /special value/, 1 is returned only if both operands
-- are NaNs or both are infinities.
--
-- 'sameQuantum' does not change any /flags/ in the context.
sameQuantum :: Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
sameQuantum Num { exponent = e1 } Num { exponent = e2 }
  | e1 == e2  = return one
  | otherwise = return zero
sameQuantum Inf {} Inf {} = return one
sameQuantum QNaN{} QNaN{} = return one
sameQuantum SNaN{} SNaN{} = return one
sameQuantum QNaN{} SNaN{} = return one
sameQuantum SNaN{} QNaN{} = return one
sameQuantum _      _      = return zero

{- $doctest-sameQuantum
>>> op2 Op.sameQuantum "2.17" "0.001"
0

>>> op2 Op.sameQuantum "2.17" "0.01"
1

>>> op2 Op.sameQuantum "2.17" "0.1"
0

>>> op2 Op.sameQuantum "2.17" "1"
0

>>> op2 Op.sameQuantum "Inf" "-Inf"
1

>>> op2 Op.sameQuantum "NaN" "NaN"
1
-}
