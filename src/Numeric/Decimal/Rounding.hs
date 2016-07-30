
module Numeric.Decimal.Rounding
       ( RoundingAlgorithm(..)
       , Rounding(..)

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

import Data.Coerce (coerce)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Arithmetic

data RoundingAlgorithm = RoundDown
                       | RoundHalfUp
                       | RoundHalfEven
                       | RoundCeiling
                       | RoundFloor
                       | RoundHalfDown
                       | RoundUp
                       | Round05Up
                       deriving (Eq, Enum)

-- | A rounding algorithm to use when the result of an arithmetic operation
-- exceeds the precision of the result type
class Rounding r where
  rounding :: r -> RoundingAlgorithm
  round    :: Precision p => Decimal a b -> Arith p r (Decimal p r)

-- Required...

-- | Round toward 0 (truncate)
data RoundDown
instance Rounding RoundDown where
  rounding _ = RoundDown
  round = roundDown

-- | If the discarded digits represent greater than or equal to half (0.5) of
-- the value of a one in the next left position then the value is rounded
-- up. If they represent less than half, the value is rounded down.
data RoundHalfUp
instance Rounding RoundHalfUp where
  rounding _ = RoundHalfUp
  round = roundHalfUp

-- | If the discarded digits represent greater than half (0.5) of the value of
-- a one in the next left position then the value is rounded up. If they
-- represent less than half, the value is rounded down. If they represent
-- exactly half, the value is rounded to make its rightmost digit even.
data RoundHalfEven
instance Rounding RoundHalfEven where
  rounding _ = RoundHalfEven
  round = roundHalfEven

-- | Round toward +∞
data RoundCeiling
instance Rounding RoundCeiling where
  rounding _ = RoundCeiling
  round = roundCeiling

-- | Round toward −∞
data RoundFloor
instance Rounding RoundFloor where
  rounding _ = RoundFloor
  round = roundFloor

-- Optional...

-- | If the discarded digits represent greater than half (0.5) of the value of
-- a one in the next left position then the value is rounded up. If they
-- represent less than half or exactly half, the value is rounded down.
data RoundHalfDown
instance Rounding RoundHalfDown where
  rounding _ = RoundHalfDown
  round = roundHalfDown

-- | Round away from 0
data RoundUp
instance Rounding RoundUp where
  rounding _ = RoundUp
  round = roundUp

-- | Round zero or five away from 0
data Round05Up
instance Rounding Round05Up where
  rounding _ = Round05Up
  round = round05Up

-- Implementations

excessDigits :: Precision p => Decimal a b -> Arith p r (Maybe Int)
excessDigits Num { coefficient = c } = do
  p <- getPrecision
  return (p >>= excess)
  where d = numDigits c
        excess p
          | d > p     = Just (d - p)
          | otherwise = Nothing
excessDigits _ = return Nothing

rounded :: (Coefficient -> Coefficient -> Coefficient ->
            Decimal p r -> Decimal p r -> Decimal p r)
        -> Int -> Decimal p r -> Arith p r (Decimal p r)
rounded f d n = raiseSignal Rounded =<< rounded' n'
  where rounded'
          | r /= 0    = raiseSignal Inexact
          | otherwise = return
        p = 10 ^ d
        (q, r) = coefficient n `quotRem` p
        n' = f (p `quot` 2) q r down up
        down = n { coefficient = q    , exponent = exponent n + fromIntegral d }
        up   = n { coefficient = q + 1, exponent = exponent n + fromIntegral d }

roundDown :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundDown n = excessDigits n >>= roundDown'
  where roundDown' Nothing  = return (coerce n)
        roundDown' (Just d) = rounded choice d (coerce n)

        choice _h _q _r down _up = down

roundHalfUp :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundHalfUp n = excessDigits n >>= roundHalfUp'
  where roundHalfUp' Nothing  = return (coerce n)
        roundHalfUp' (Just d) = rounded choice d (coerce n)

        choice h _q r down up
          | r >= h    = up
          | otherwise = down

roundHalfEven :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundHalfEven n = excessDigits n >>= roundHalfEven'
  where roundHalfEven' Nothing  = return (coerce n)
        roundHalfEven' (Just d) = rounded choice d (coerce n)

        choice h q r down up = case r `Prelude.compare` h of
          LT -> down
          GT -> up
          EQ | even q    -> down
             | otherwise -> up

roundCeiling :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundCeiling n = excessDigits n >>= roundCeiling'
  where roundCeiling' Nothing  = return (coerce n)
        roundCeiling' (Just d) = rounded choice d (coerce n)

        choice _h _q r down up
          | r == 0 || sign n == Neg = down
          | otherwise               = up

roundFloor :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundFloor n = excessDigits n >>= roundFloor'
  where roundFloor' Nothing  = return (coerce n)
        roundFloor' (Just d) = rounded choice d (coerce n)

        choice _h _q r down up
          | r == 0 || sign n == Pos = down
          | otherwise               = up

roundHalfDown :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundHalfDown n = excessDigits n >>= roundHalfDown'
  where roundHalfDown' Nothing  = return (coerce n)
        roundHalfDown' (Just d) = rounded choice d (coerce n)

        choice h _q r down up
          | r > h     = up
          | otherwise = down

roundUp :: Precision p => Decimal a b -> Arith p r (Decimal p r)
roundUp n = excessDigits n >>= roundUp'
  where roundUp' Nothing  = return (coerce n)
        roundUp' (Just d) = rounded choice d (coerce n)

        choice _h _q r down up
          | r == 0    = down
          | otherwise = up

round05Up :: Precision p => Decimal a b -> Arith p r (Decimal p r)
round05Up n = excessDigits n >>= round05Up'
  where round05Up' Nothing  = return (coerce n)
        round05Up' (Just d) = rounded choice d (coerce n)

        choice _h q r down up
          | r == 0           = down
          | d == 0 || d == 5 = up  -- XXX overflow -> roundDown?
          | otherwise        = down
          where d = q `rem` 10
