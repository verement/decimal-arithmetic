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

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Rounding

add      :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
subtract :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
multiply :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
divide   :: (FinitePrecision p, Rounding r) =>
                                         Number p r -> Number p r -> Number p r
minus    :: (Precision p, Rounding r) => Number p r -> Number p r
abs      :: (Precision p, Rounding r) => Number p r -> Number p r
compare  :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
