
module Numeric.Decimal.Operations
       ( add
       , subtract
       , multiply
       , divide
       , minus
       , plus
       , abs
       ) where

import Prelude hiding (abs, exponent, round, subtract)
import qualified Prelude

import                Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Rounding

invalidOperation :: Number p r -> Number p r
invalidOperation n = raiseSignal InvalidOperation qNaN { context = context n }

qNaN :: Number p r
qNaN = QNaN { context = defaultContext, sign = Pos, payload = 0 }

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

subtract :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
subtract x = add x . flipSign

minus :: (Precision p, Rounding r) => Number p r -> Number p r
minus x = zero { exponent = exponent x } `subtract` x

plus :: (Precision p, Rounding r) => Number p r -> Number p r
plus x = zero { exponent = exponent x } `add` x

xorSigns :: Sign -> Sign -> Sign
xorSigns Pos Pos = Pos
xorSigns Pos Neg = Neg
xorSigns Neg Pos = Neg
xorSigns Neg Neg = Pos

multiply :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
multiply Num { context = xt, sign = xs, coefficient = xc, exponent = xe }
         Num { context = yt, sign = ys, coefficient = yc, exponent = ye } =
  round rn

  where rn = Num { context = rt, sign = rs, coefficient = rc, exponent = re }
        rt = mergeContexts xt yt
        rc = xc * yc
        re = xe + ye
        rs = xorSigns xs ys

multiply Inf { context = xt, sign = xs } Inf { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply Inf { context = xt, sign = xs } Num { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply Num { context = xt, sign = xs } Inf { context = yt, sign = ys } =
  Inf { context = mergeContexts xt yt, sign = xorSigns xs ys }
multiply x y = toQNaN2 x y

divide :: Number p r -> Number p r -> Number p r
divide = undefined

abs :: (Precision p, Rounding r) => Number p r -> Number p r
abs x
  | isNegative x = minus x
  | otherwise    = plus  x
