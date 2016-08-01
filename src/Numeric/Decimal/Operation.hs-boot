-- -*- Haskell -*-

module Numeric.Decimal.Operation
       ( add
       , subtract
       , multiply
       , divide
       , exp
       , ln
       , log10
       , minus
       , abs
       , compare
       , min
       , max
       , power
       , squareRoot
       , and
       , or
       , xor
       , invert
       , shift
       , rotate
       ) where

import Prelude hiding (abs, and, compare, exp, max, min, or, subtract)

import {-# SOURCE #-} Numeric.Decimal.Arithmetic
import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import                Numeric.Decimal.Rounding

add      :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
subtract :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
multiply :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
divide   :: (FinitePrecision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
exp      :: FinitePrecision p
         => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
ln       :: FinitePrecision p
         => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
log10    :: FinitePrecision p
         => Decimal a b -> Arith p r (Decimal p RoundHalfEven)
minus    :: (Precision p, Rounding r)
         => Decimal a b -> Arith p r (Decimal p r)
abs      :: (Precision p, Rounding r)
         => Decimal a b -> Arith p r (Decimal p r)
compare  :: (Precision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
min      :: (Precision p, Rounding r)
         => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
max      :: (Precision p, Rounding r)
         => Decimal a b -> Decimal a b -> Arith p r (Decimal a b)
power    :: (FinitePrecision p, Rounding r)
         => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
squareRoot :: FinitePrecision p
           => Decimal a b -> Arith p r (Decimal p RoundHalfEven)

and    :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
or     :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
xor    :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
invert :: FinitePrecision p => Decimal a b -> Arith p r (Decimal p r)
shift  :: Precision p => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
rotate :: FinitePrecision p
       => Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
