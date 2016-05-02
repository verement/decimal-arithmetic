-- -*- Haskell -*-

module Numeric.Decimal.Operations
       ( add
       , subtract
       , multiply
       , minus
       , abs
       ) where

import Prelude hiding (abs, subtract)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Rounding

add      :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
subtract :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
multiply :: (Precision p, Rounding r) => Number p r -> Number p r -> Number p r
minus    :: (Precision p, Rounding r) => Number p r -> Number p r
abs      :: (Precision p, Rounding r) => Number p r -> Number p r
