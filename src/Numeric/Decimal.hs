
module Numeric.Decimal
       ( Number
       , BasicDecimal
       , ExtendedDecimal
       , GeneralDecimal
       ) where

import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

type BasicDecimal      = Number P9 RoundHalfUp
type ExtendedDecimal p = Number p  RoundHalfEven

type GeneralDecimal = ExtendedDecimal PInfinite

basicDefaultContext :: TrapHandler P9 RoundHalfUp -> Context P9 RoundHalfUp
basicDefaultContext handler = defaultContext { trapHandler = trap }
  where trap Inexact   = id
        trap Rounded   = id
        trap Subnormal = id
        trap sig       = handler sig

extendedDefaultContext :: Context p RoundHalfEven
extendedDefaultContext = defaultContext
