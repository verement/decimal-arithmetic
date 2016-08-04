
module Numeric.Decimal.Exception (
    inexact
  , rounded
  ) where

import {-# SOURCE #-} Numeric.Decimal.Arithmetic
import {-# SOURCE #-} Numeric.Decimal.Number

inexact :: Decimal p r -> Arith p r (Decimal p r)
rounded :: Decimal p r -> Arith p r (Decimal p r)
