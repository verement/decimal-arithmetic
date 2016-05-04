
module Numeric.Decimal.Arbitrary
       ( Arbitrary
       ) where

import Numeric.Decimal
import Test.QuickCheck

infinity :: (Precision p, Rounding r) => Number p r
infinity = read "Infinity"

instance (Precision p, Rounding r) => Arbitrary (Number p r) where
  arbitrary = frequency [(85, genNum), (10, genInf)]

genNum :: (Precision p, Rounding r) => Gen (Number p r)
genNum = do
  c <- choose (-(10^10), 10^10) :: Gen Integer
  e <- choose (-99, 99)         :: Gen Integer
  return $ read (show c ++ 'E' : show e)

genInf :: (Precision p, Rounding r) => Gen (Number p r)
genInf = do
  s <- elements [-1, 1]
  return (s * infinity)

genNaN :: (Precision p, Rounding r) => Gen (Number p r)
genNaN = oneof [nan "", nan "s"]
  where nan kind = do
          s <- elements ["", "-"]
          p <- choose (0, 10000) :: Gen Integer
          return $ read (s ++ kind ++ "NaN" ++ show p)
