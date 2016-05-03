
-- | Eventually most or all of the arithmetic operations described in the
-- /General Decimal Arithmetic Specification/ will be provided here. For now,
-- the operations are mostly limited to those exposed through various class
-- methods.
--
-- It is not usually necessary to import this module.

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

import                Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Rounding

invalidOperation :: Number p r -> Number p r
invalidOperation n = raiseSignal InvalidOperation qNaN { context = context n }

toQNaN :: Number p r -> Number p r
toQNaN SNaN { context = t, sign = s, payload = p } =
  QNaN { context = t, sign = s, payload = p }
toQNaN n@QNaN{} = n
toQNaN n = qNaN { context = context n, sign = sign n }

toQNaN2 :: Number p r -> Number p r -> Number p r
toQNaN2 nan@SNaN{} _ = toQNaN nan
toQNaN2 _ nan@SNaN{} = toQNaN nan
toQNaN2 nan@QNaN{} _ = nan
toQNaN2 _ nan@QNaN{} = nan
toQNaN2 n _          = toQNaN n

-- | Add two operands.
add :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
add Num { context = xt, sign = xs, coefficient = xc, exponent = xe }
    Num { context = yt, sign = ys, coefficient = yc, exponent = ye } = round rn

  where rn = Num { context = rt, sign = rs, coefficient = rc, exponent = re }
        rt = mergeContexts xt yt
        rs | rc /= 0                     = if xac > yac then xs else ys
           | xs == Neg && ys == Neg      = Neg
           | xs /= ys && isRoundFloor rn = Neg
           | otherwise                   = Pos
        rc | xs == ys  = xac + yac
           | xac > yac = xac - yac
           | otherwise = yac - xac
        re = min xe ye
        (xac, yac) | xe == ye  = (xc, yc)
                   | xe >  ye  = (xc * 10^n, yc)
                   | otherwise = (xc, yc * 10^n)
          where n = Prelude.abs (xe - ye)

add inf@Inf { context = xt, sign = xs } Inf { context = yt, sign = ys }
  | xs == ys  = inf { context = mergeContexts xt yt }
  | otherwise = invalidOperation inf { context = mergeContexts xt yt }
add inf@Inf{} Num{} = inf
add Num{} inf@Inf{} = inf
add x y             = toQNaN2 x y

-- | Subtract the second operand from the first.
subtract :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
subtract x = add x . flipSign

-- | Unary minus (negation)
minus :: (Precision p, Rounding r) => Number p r -> Number p r
minus x = zero { exponent = exponent x } `subtract` x

-- | Unary plus
plus :: (Precision p, Rounding r) => Number p r -> Number p r
plus x = zero { exponent = exponent x } `add` x

-- | Multiply two operands.
multiply :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
multiply Num { context = xt, sign = xs, coefficient = xc, exponent = xe }
         Num { context = yt, sign = ys, coefficient = yc, exponent = ye } =
  round rn

  where rn = Num { context = rt, sign = rs, coefficient = rc, exponent = re }
        rt = mergeContexts xt yt
        rs = xorSigns xs ys
        rc = xc * yc
        re = xe + ye

multiply Inf { context = xt, sign = xs } Inf { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply Inf { context = xt, sign = xs } Num { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply Num { context = xt, sign = xs } Inf { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply x y = toQNaN2 x y

-- | Divide the first dividend operand by the second divisor using long division.
divide :: (FinitePrecision p, Rounding r)
       => Number p r -> Number p r -> Number p r
divide dividend@Num{ sign = xs } Num { coefficient = 0, sign = ys }
  | isZero dividend = invalidOperation qNaN
  | otherwise       = raiseSignal DivisionByZero
                        infinity { sign = xorSigns xs ys }
divide Num { context = xt, sign = xs, coefficient = xc, exponent = xe }
       Num { context = yt, sign = ys, coefficient = yc, exponent = ye } =
  result

  where rn = Num { context = rt, sign = rs, coefficient = rc, exponent = re }
        rt = mergeContexts xt yt
        rs = xorSigns xs ys
        (rc, rem, dv, adjust) = longDivision xc yc p
        re = xe - (ye + adjust)
        Just p = precision rn
        result
          | rem == 0  = rn
          | otherwise = round $ case (rem * 2) `Prelude.compare` dv of
              LT -> rn { coefficient = rc * 10 + 1, exponent = re - 1 }
              EQ -> rn { coefficient = rc * 10 + 5, exponent = re - 1 }
              GT -> rn { coefficient = rc * 10 + 9, exponent = re - 1 }

divide Inf{} Inf{} = invalidOperation qNaN
divide Inf { context = xt, sign = xs } Num { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
divide Num { context = xt, sign = xs } Inf { context = yt, sign = ys } =
  zero { context = mergeContexts xt yt, sign = xorSigns xs ys }
divide x y = toQNaN2 x y

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
abs :: (Precision p, Rounding r) => Number p r -> Number p r
abs x
  | isNegative x = minus x
  | otherwise    = plus  x

-- | Compare the values of two operands numerically, returning @-1@ if the
-- first is less than the second, @0@ if they are equal, or @1@ if the first
-- is greater than the second.
compare :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
compare x@Num{} y@Num{} = (nzp $ xn `subtract` yn) { context = rt }

  where (xn, yn) | sign x /= sign y = (nzp x, nzp y)
                 | otherwise        = (x, y)

        rt = mergeContexts (context x) (context y)

        nzp :: Number p r -> Number p r
        nzp Num { context = t, sign = s, coefficient = c }
          | c == 0    = zero        { context = t }
          | s == Pos  = one         { context = t }
          | otherwise = negativeOne { context = t }
        nzp Inf { context = t, sign = s }
          | s == Pos  = one         { context = t }
          | otherwise = negativeOne { context = t }
        nzp n = toQNaN n

compare Inf { context = xt, sign = xs } Inf { context = yt, sign = ys }
  | xs == ys  = zero        { context = rt }
  | xs == Neg = negativeOne { context = rt }
  | otherwise = one         { context = rt }
  where rt = mergeContexts xt yt
compare Inf { context = xt, sign = xs } Num { context = yt }
  | xs == Neg = negativeOne { context = rt }
  | otherwise = one         { context = rt }
  where rt = mergeContexts xt yt
compare Num { context = xt } Inf { context = yt, sign = ys }
  | ys == Pos = negativeOne { context = rt }
  | otherwise = one         { context = rt }
  where rt = mergeContexts xt yt
compare nan@SNaN{} _ = invalidOperation nan
compare _ nan@SNaN{} = invalidOperation nan
compare x y          = toQNaN2 x y
