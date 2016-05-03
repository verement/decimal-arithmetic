
module Numeric.Decimal.Rounding
       ( Rounding(..)

       , RoundDown
       , RoundHalfUp
       , RoundHalfEven
       , RoundCeiling
       , RoundFloor

       , RoundHalfDown
       , RoundUp
       , Round05Up
       ) where

import Prelude hiding (exponent)

import Numeric.Decimal.Number
import Numeric.Decimal.Precision

-- | A rounding algorithm to use when the result of an arithmetic operation
-- exceeds the precision of the result type
class Rounding r where
  round :: Precision p => Number p r -> Number p r

  isRoundFloor :: Number p r -> Bool
  isRoundFloor _ = False

-- Required...

-- | Round toward 0 (truncate)
data RoundDown
instance Rounding RoundDown where
  round = roundDown

-- | If the discarded digits represent greater than or equal to half (0.5) of
-- the value of a one in the next left position then the value is rounded
-- up. If they represent less than half, the value is rounded down.
data RoundHalfUp
instance Rounding RoundHalfUp where
  round = roundHalfUp

-- | If the discarded digits represent greater than half (0.5) of the value of
-- a one in the next left position then the value is rounded up. If they
-- represent less than half, the value is rounded down. If they represent
-- exactly half, the value is rounded to make its rightmost digit even.
data RoundHalfEven
instance Rounding RoundHalfEven where
  round = roundHalfEven

-- | Round toward +∞
data RoundCeiling
instance Rounding RoundCeiling where
  round = roundCeiling

-- | Round toward −∞
data RoundFloor
instance Rounding RoundFloor where
  round = roundFloor
  isRoundFloor _ = True

-- Optional...

-- | If the discarded digits represent greater than half (0.5) of the value of
-- a one in the next left position then the value is rounded up. If they
-- represent less than half or exactly half, the value is rounded down.
data RoundHalfDown
instance Rounding RoundHalfDown where
  round = roundHalfDown

-- | Round away from 0
data RoundUp
instance Rounding RoundUp where
  round = roundUp

-- | Round zero or five away from 0
data Round05Up
instance Rounding Round05Up where
  round = round05Up

-- Implementations

rounded :: (Coefficient -> Coefficient -> Coefficient ->
            Number p r -> Number p r -> Number p r)
        -> Int -> Number p r -> Number p r
rounded f d n = raiseSignal Rounded rounded'
  where rounded'
          | r /= 0    = raiseSignal Inexact n'
          | otherwise = n'
        p = 10 ^ d
        (q, r) = coefficient n `quotRem` p
        n' = f (p `quot` 2) q r down up
        down = n { coefficient = q
                 , exponent = exponent n + fromIntegral d
                 }
        up = n { coefficient = q + 1
               , exponent = exponent n + fromIntegral d
               }

roundDown :: Precision p => Number p r -> Number p r
roundDown n = roundDown' (excessDigits n)
  where roundDown' Nothing  = n
        roundDown' (Just d) = rounded choice d n

        choice _h _q _r down _up = down

roundHalfUp :: Precision p => Number p r -> Number p r
roundHalfUp n = roundHalfUp' (excessDigits n)
  where roundHalfUp' Nothing  = n
        roundHalfUp' (Just d) = rounded choice d n

        choice h _q r down up
          | r >= h    = roundHalfUp up
          | otherwise = down

roundHalfEven :: Precision p => Number p r -> Number p r
roundHalfEven n = roundHalfEven' (excessDigits n)
  where roundHalfEven' Nothing  = n
        roundHalfEven' (Just d) = rounded choice d n

        choice h q r down up = case r `Prelude.compare` h of
          LT -> down
          GT -> roundHalfEven up
          EQ | even q    -> down
             | otherwise -> roundHalfEven up

roundCeiling :: Precision p => Number p r -> Number p r
roundCeiling n = roundCeiling' (excessDigits n)
  where roundCeiling' Nothing  = n
        roundCeiling' (Just d) = rounded choice d n

        choice _h _q r down up
          | r == 0 || sign n == Neg = down
          | otherwise               = roundCeiling up

roundFloor :: Precision p => Number p r -> Number p r
roundFloor n = roundFloor' (excessDigits n)
  where roundFloor' Nothing  = n
        roundFloor' (Just d) = rounded choice d n

        choice _h _q r down up
          | r == 0 || sign n == Pos = down
          | otherwise               = roundFloor up

roundHalfDown :: Precision p => Number p r -> Number p r
roundHalfDown n = roundHalfDown' (excessDigits n)
  where roundHalfDown' Nothing  = n
        roundHalfDown' (Just d) = rounded choice d n

        choice h _q r down up
          | r > h     = roundHalfDown up
          | otherwise = down

roundUp :: Precision p => Number p r -> Number p r
roundUp n = roundUp' (excessDigits n)
  where roundUp' Nothing  = n
        roundUp' (Just d) = rounded choice d n

        choice _h _q r down up
          | r == 0    = down
          | otherwise = roundUp up

round05Up :: Precision p => Number p r -> Number p r
round05Up n = round05Up' (excessDigits n)
  where round05Up' Nothing  = n
        round05Up' (Just d) = rounded choice d n

        choice _h q r down up
          | r == 0           = down
          | d == 0 || d == 5 = round05Up up  -- overflow -> roundDown?
          | otherwise        = down
          where d = q `rem` 10
