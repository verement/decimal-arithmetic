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
       ) where

import Prelude hiding (abs, compare, exp, max, min, subtract)

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
