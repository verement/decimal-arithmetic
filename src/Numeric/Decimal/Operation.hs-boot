-- -*- Haskell -*-

module Numeric.Decimal.Operation
       ( add
       , subtract
       , multiply
       , divide
       , minus
       , abs
       , compare
       ) where

import Prelude hiding (abs, compare, subtract)

import {-# SOURCE #-} Numeric.Decimal.Arithmetic
import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import                Numeric.Decimal.Rounding

add      :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
subtract :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
multiply :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
divide   :: (FinitePrecision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
minus    :: (Precision p, Rounding r)
         => Number a b -> Arith p r (Number p r)
abs      :: (Precision p, Rounding r)
         => Number a b -> Arith p r (Number p r)
compare  :: (Precision p, Rounding r)
         => Number a b -> Number c d -> Arith p r (Number p r)
