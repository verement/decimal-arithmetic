-- -*- Haskell -*-

module Numeric.Decimal.Rounding
       ( Rounding(..)
       ) where

import {-# SOURCE #-} Numeric.Decimal.Number (Number)
import                Numeric.Decimal.Precision (Precision)

class Rounding r where
  round :: Precision p => Number p r -> Number p r

  isRoundFloor :: Number p r -> Bool
  isRoundFloor _ = False
