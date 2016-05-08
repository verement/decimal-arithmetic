
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
       ( abs
       , add
       , subtract
       , multiply
       , divide
       , plus
       , minus
       , compare
       ) where

import Prelude hiding (abs, compare, exponent, round, subtract)
import qualified Prelude

import Data.Coerce (coerce)

import Numeric.Decimal.Arithmetic
import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

finitePrecision :: FinitePrecision p => Number p r -> Int
finitePrecision n = let Just p = precision n in p

roundingAlg :: Rounding r => Arith p r a -> RoundingAlgorithm
roundingAlg = rounding . arithRounding
  where arithRounding :: Arith p r a -> r
        arithRounding = undefined

result :: (Precision p, Rounding r) => Number p r -> Arith p r (Number p r)
result = round  -- ...
--  | maybe False (numDigits c >) (precision r) = undefined

invalidOperation :: Number a b -> Arith p r (Number p r)
invalidOperation n = raiseSignal InvalidOperation qNaN

toQNaN :: Number a b -> Number p r
toQNaN SNaN { sign = s, payload = p } =
  QNaN { sign = s, payload = p }
toQNaN n@QNaN{} = coerce n
toQNaN n = qNaN { sign = sign n }

toQNaN2 :: Number a b -> Number c d -> Number p r
toQNaN2 nan@SNaN{} _ = toQNaN nan
toQNaN2 _ nan@SNaN{} = toQNaN nan
toQNaN2 nan@QNaN{} _ = coerce nan
toQNaN2 _ nan@QNaN{} = coerce nan
toQNaN2 n _          = toQNaN n

-- | Add two operands.
add :: (Precision p, Rounding r)
    => Number a b -> Number c d -> Arith p r (Number p r)
add Num { sign = xs, coefficient = xc, exponent = xe }
    Num { sign = ys, coefficient = yc, exponent = ye } = sum

  where sum = result rn
        rn = Num { sign = rs, coefficient = rc, exponent = re }
        rs | rc /= 0                       = if xac > yac then xs else ys
           | xs == Neg && ys == Neg        = Neg
           | xs /= ys &&
             roundingAlg sum == RoundFloor = Neg
           | otherwise                     = Pos
        rc | xs == ys  = xac + yac
           | xac > yac = xac - yac
           | otherwise = yac - xac
        re = min xe ye
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

-- | Subtract the second operand from the first.
subtract :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
subtract x = add x . flipSign

-- | Unary minus (negation)
minus :: (Precision p, Rounding r) => Number a b -> Arith p r (Number p r)
minus x = zero { exponent = exponent x } `subtract` x

-- | Unary plus
plus :: (Precision p, Rounding r) => Number a b -> Arith p r (Number p r)
plus x = zero { exponent = exponent x } `add` x

-- | Multiply two operands.
multiply :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
multiply Num { sign = xs, coefficient = xc, exponent = xe }
         Num { sign = ys, coefficient = yc, exponent = ye } =
  result rn

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

-- | Divide the first dividend operand by the second divisor using long division.
divide :: (FinitePrecision p, Rounding r)
       => Number a b -> Number c d -> Arith p r (Number p r)
divide dividend@Num{ sign = xs } Num { coefficient = 0, sign = ys }
  | isZero dividend = invalidOperation qNaN
  | otherwise       = raiseSignal DivisionByZero
                        infinity { sign = xorSigns xs ys }
divide Num { sign = xs, coefficient = xc, exponent = xe }
       Num { sign = ys, coefficient = yc, exponent = ye } =
  quotient

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

type Dividend  = Coefficient
type Divisor   = Coefficient
type Quotient  = Coefficient
type Remainder = Coefficient

longDivision :: Dividend -> Divisor -> Int
             -> (Quotient, Remainder, Divisor, Exponent)
longDivision 0  dv _ = (0, 0, dv, 0)
longDivision dd dv p = step1 dd dv 0

  where step1 dd dv adjust
          | dd <       dv = step1 (dd * 10)  dv       (adjust + 1)
          | dd >= 10 * dv = step1  dd       (dv * 10) (adjust - 1)
          | otherwise     = step2  dd        dv        adjust

        step2 = step3 0

        step3 r dd dv adjust
          | dv <= dd                 = step3 (r +  1) (dd - dv) dv  adjust
          | (dd == 0 && adjust >= 0) ||
            numDigits r == p         = step4  r        dd       dv  adjust
          | otherwise                = step3 (r * 10) (dd * 10) dv (adjust + 1)

        step4 = (,,,)

-- | If the operand is negative, the result is the same as using the 'minus'
-- operation on the operand. Otherwise, the result is the same as using the
-- 'plus' operation on the operand.
abs :: (Precision p, Rounding r) => Number a b -> Arith p r (Number p r)
abs x
  | isNegative x = minus x
  | otherwise    = plus  x

-- | Compare the values of two operands numerically, returning @-1@ if the
-- first is less than the second, @0@ if they are equal, or @1@ if the first
-- is greater than the second.
compare :: (Precision p, Rounding r)
        => Number a b -> Number c d -> Arith p r (Number p r)
compare x@Num{} y@Num{} = nzp <$> (xn `subtract` yn)

  where (xn, yn) | sign x /= sign y = (nzp x, nzp y)
                 | otherwise        = (x, y)

        nzp :: Number p r -> Number p r
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