{-|
Module      : Numeric.Decimal
Description : General arbitrary-precision decimal floating-point number type
Copyright   : © 2016 Robert Leslie
License     : BSD3
Maintainer  : rob@mars.org
Stability   : experimental

This module provides a general-purpose 'Number' type supporting decimal
arithmetic for both limited precision floating-point (IEEE 754-2008) and for
arbitrary precision floating-point (following the same principles as IEEE 754
and IEEE 854-1987) as described in the
<http://speleotrove.com/decimal/ General Decimal Arithmetic Specification>
by Mike Cowlishaw. In addition to floating-point arithmetic, integer and
unrounded floating-point arithmetic are included as subsets.

Unlike the binary floating-point types 'Float' and 'Double', the 'Number' type
can represent and perform arithmetic with decimal numbers exactly.
Internally, a 'Number' is represented with an integral coefficient and base-10
exponent.

The 'Number' type supports lossless conversion to and from a string
representation via the 'Show' and 'Read' instances. Note that there may be
multiple representations of values that are numerically equal (e.g. 1 and
1.00) which are preserved by this conversion.
-}
module Numeric.Decimal
       ( -- * Usage
         -- $usage

         -- * Arbitrary-precision decimal numbers
         Number
       , BasicDecimal
       , ExtendedDecimal
       , GeneralDecimal

         -- ** Precision types
       , module Numeric.Decimal.Precision

         -- ** Rounding types
       , Rounding

       , RoundHalfUp
       , RoundHalfEven
       , RoundHalfDown
       , RoundCeiling
       , RoundFloor
       , RoundUp
       , Round05Up
       , RoundDown

         -- * Functions
       , cast
       ) where

import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

-- | A basic decimal floating point number with 9 digits of precision, rounding half up
type BasicDecimal = Number P9 RoundHalfUp

-- | A decimal floating point number with selectable precision, rounding half even
type ExtendedDecimal p = Number p  RoundHalfEven

-- | A decimal floating point number with infinite precision
type GeneralDecimal = ExtendedDecimal PInfinite

basicDefaultContext :: TrapHandler P9 RoundHalfUp -> Context P9 RoundHalfUp
basicDefaultContext handler = defaultContext { trapHandler = trap }
  where trap Inexact   = id
        trap Rounded   = id
        trap Subnormal = id
        trap sig       = handler sig

extendedDefaultContext :: Context p RoundHalfEven
extendedDefaultContext = defaultContext

-- $usage
--
-- It is recommended to create an alias for the type of numbers you wish to
-- support in your application. For example:
--
-- >  type Decimal = BasicDecimal
--
-- This is a basic number type with 9 decimal digits of precision that rounds
-- half up.
--
-- >  type Decimal = ExtendedDecimal P15
--
-- This is a number type with 15 decimal digits of precision that rounds half
-- even. There are a range of ready-made precisions available, including 'P1'
-- through 'P50' on up to 'P2000'. Alternatively, an arbitrary precision can
-- be constructed through type application of 'PPlus1' and/or 'PTimes2' to any
-- existing precision.
--
-- >  type Decimal = GeneralDecimal
--
-- This is a number type with infinite precision. Note that not all operations
-- support numbers with infinite precision.
--
-- >  type Decimal = Number P34 RoundDown
--
-- This is a custom number type with 34 decimal digits of precision that
-- rounds down (truncates). Several 'Rounding' algorithms are available to
-- choose from.
--
-- A decimal number type may be used in a @default@ declaration, possibly
-- replacing 'Double' or 'Integer'. For example:
--
-- >  default (Integer, Decimal)
