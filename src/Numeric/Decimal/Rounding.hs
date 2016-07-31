
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

       , getRounder
       , roundDecimal
       ) where

import Prelude hiding (exponent)

import Data.Coerce (coerce)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Arithmetic

-- | A value representation of a rounding algorithm (cf. 'Rounding').
data RoundingAlgorithm = RoundDown
                       | RoundHalfUp
                       | RoundHalfEven
                       | RoundCeiling
                       | RoundFloor
                       | RoundHalfDown
                       | RoundUp
                       | Round05Up
                       deriving Eq

-- | A rounding algorithm to use when the result of an arithmetic operation
-- exceeds the precision of the result type
class Rounding r where
  rounding         :: r -> RoundingAlgorithm
  roundCoefficient :: r -> Rounder

type Remainder = Coefficient
type Divisor   = Coefficient

type Rounder = Sign -> Remainder -> Divisor -> Coefficient -> Coefficient

getRounder :: Rounding r => Arith p r Rounder
getRounder = ($ undefined) <$> getRounder'
  where getRounder' :: Rounding r => Arith p r (r -> Rounder)
        getRounder' = return roundCoefficient

-- | Round a 'Decimal' to the precision of the arithmetic context using the
-- rounding algorithm of the arithmetic context.
roundDecimal :: (Precision p, Rounding r)
             => Decimal a b -> Arith p r (Decimal p r)
roundDecimal n@Num { sign = s, coefficient = c, exponent = e } = do
  p <- getPrecision
  case excessDigits c =<< p of
    Just d -> do
      rounder <- getRounder
      let b      = 10 ^ d
          (q, r) = c `quotRem` b
          c'     = rounder s r b q
          e'     = e + fromIntegral d
          n'     = case excessDigits c' =<< p of
            Nothing -> n { coefficient = c'          , exponent =      e' }
            _       -> n { coefficient = c' `quot` 10, exponent = succ e' }
          rounded :: Decimal p r -> Arith p r (Decimal p r)
          rounded
            | r /= 0    = raiseSignal Inexact
            | otherwise = return
      raiseSignal Rounded =<< rounded n'  -- XXX check for overflow

    Nothing -> return (coerce n)

  where excessDigits :: Coefficient -> Int -> Maybe Int
        excessDigits c p | d > p     = Just (d - p)
                         | otherwise = Nothing
          where d = numDigits c :: Int

roundDecimal n = return (coerce n)

-- Required algorithms...

-- | (Round toward 0; truncate.) The discarded digits are ignored; the result
-- is unchanged.
data RoundDown
instance Rounding RoundDown where
  rounding _ = RoundDown

  roundCoefficient _ _ _ _ = id

-- | If the discarded digits represent greater than or equal to half (0.5) of
-- the value of a one in the next left position then the result coefficient
-- should be incremented by 1 (rounded up). Otherwise the discarded digits are
-- ignored.
data RoundHalfUp
instance Rounding RoundHalfUp where
  rounding _ = RoundHalfUp

  roundCoefficient _ _ r v | r * 2 >= v = succ
                           | otherwise  = id

-- | If the discarded digits represent greater than half (0.5) the value of a
-- one in the next left position then the result coefficient should be
-- incremented by 1 (rounded up). If they represent less than half, then the
-- result coefficient is not adjusted (that is, the discarded digits are
-- ignored).
--
-- Otherwise (they represent exactly half) the result coefficient is unaltered
-- if its rightmost digit is even, or incremented by 1 (rounded up) if its
-- rightmost digit is odd (to make an even digit).
data RoundHalfEven
instance Rounding RoundHalfEven where
  rounding _ = RoundHalfEven

  roundCoefficient _ _ r v q = case (r * 2) `Prelude.compare` v of
    GT         -> succ q
    EQ | odd q -> succ q
    _          ->      q

-- | (Round toward +∞.) If all of the discarded digits are zero or if the
-- /sign/ is 1 the result is unchanged. Otherwise, the result coefficient
-- should be incremented by 1 (rounded up).
data RoundCeiling
instance Rounding RoundCeiling where
  rounding _ = RoundCeiling

  roundCoefficient _ Pos r _ | r /= 0 = succ
  roundCoefficient _ _   _ _          = id

-- | (Round toward −∞.) If all of the discarded digits are zero or if the
-- /sign/ is 0 the result is unchanged. Otherwise, the sign is 1 and the
-- result coefficient should be incremented by 1.
data RoundFloor
instance Rounding RoundFloor where
  rounding _ = RoundFloor

  roundCoefficient _ Neg r _ | r /= 0 = succ
  roundCoefficient _ _   _ _          = id

-- Optional algorithms...

-- | If the discarded digits represent greater than half (0.5) of the value of
-- a one in the next left position then the result coefficient should be
-- incremented by 1 (rounded up). Otherwise (the discarded digits are 0.5 or
-- less) the discarded digits are ignored.
data RoundHalfDown
instance Rounding RoundHalfDown where
  rounding _ = RoundHalfDown

  roundCoefficient _ _ r v | r * 2 > v = succ
                           | otherwise = id

-- | (Round away from 0.) If all of the discarded digits are zero the result
-- is unchanged. Otherwise, the result coefficient should be incremented by 1
-- (rounded up).
data RoundUp
instance Rounding RoundUp where
  rounding _ = RoundUp

  roundCoefficient _ _ r _ | r /= 0     = succ
                           | otherwise  = id

-- | (Round zero or five away from 0.) The same as 'RoundUp', except that
-- rounding up only occurs if the digit to be rounded up is 0 or 5, and after
-- overflow the result is the same as for 'RoundDown'.
data Round05Up
instance Rounding Round05Up where
  rounding _ = Round05Up

  roundCoefficient _ _ r _ q | r /= 0 && rem q 10 `elem` [0, 5] = succ q
                             | otherwise                        =      q
