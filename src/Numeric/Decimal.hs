{-|
Module      : Numeric.Decimal
Description : General arbitrary-precision decimal floating-point number type
Copyright   : © 2016 Robert Leslie
License     : BSD3
Maintainer  : rob@mars.org
Stability   : experimental

This module provides a general-purpose number type supporting decimal
arithmetic for both limited precision floating-point (IEEE 754-2008) and for
arbitrary precision floating-point (following the same principles as IEEE 754
and IEEE 854-1987) as described in the
<http://speleotrove.com/decimal/ General Decimal Arithmetic Specification>
by Mike Cowlishaw. In addition to floating-point arithmetic, integer and
unrounded floating-point arithmetic are included as subsets.

Unlike the binary floating-point types 'Float' and 'Double', decimal number
types can perform decimal arithmetic exactly. Internally, decimal numbers are
represented with an integral coefficient and base-10 exponent.

@
@>>>@ __29.99 + 4.71 :: Double__
34.699999999999996
@

>>> 29.99 + 4.71 :: BasicDecimal
34.70

@
@>>>@ __0.1 + 0.2 == (0.3 :: Double)__
False
@

>>> 0.1 + 0.2 == (0.3 :: BasicDecimal)
True

Decimal numbers support lossless conversion to and from a string
representation via 'Show' and 'Read' instances. Note that there may be
multiple representations of values that are numerically equal (e.g. 1 and
1.00) which are preserved by this conversion.
-}
module Numeric.Decimal
       ( -- * Usage
         -- $usage

         -- * Arbitrary-precision decimal numbers
         Decimal
       , BasicDecimal
       , ExtendedDecimal
       , GeneralDecimal

         -- ** Number types with defined encodings
       , Decimal32 , toDecimal32 , fromDecimal32
       , Decimal64 , toDecimal64 , fromDecimal64
       , Decimal128, toDecimal128, fromDecimal128

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
       , fromBool
       , fromOrdering
       ) where

import Numeric.Decimal.Encoding
import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

{- $usage

You should choose a decimal number type with appropriate precision and
rounding to use in your application. There are several options:

* 'BasicDecimal' is a number type with 9 decimal digits of precision that
rounds half up.

* 'ExtendedDecimal' is a number type constructor with selectable precision
that rounds half even. For example, @'ExtendedDecimal' 'P34'@ is a number type
with 34 decimal digits of precision. There is a range of ready-made precisions
available, including 'P1' through 'P50' on up to 'P2000'. Alternatively, an
arbitrary precision can be constructed through type application of 'PPlus1'
and/or 'PTimes2' to any existing precision.

* 'GeneralDecimal' is a number type with infinite precision. Note that not all
operations support numbers with infinite precision.

* The most versatile 'Decimal' type constructor is parameterized by both a
precision and a rounding algorithm. For example, @'Decimal' 'P20' 'RoundDown'@
is a number type with 20 decimal digits of precision that rounds down
(truncates). Several 'Rounding' algorithms are available to choose from.

* 'Decimal32', 'Decimal64', and 'Decimal128' are specialized number types with
'Data.Binary.Binary' instances that implement the /decimal32/, /decimal64/,
and /decimal128/ interchange format encodings described in IEEE 754-2008.

It is suggested to create an alias for the type of numbers you wish to support
in your application. For example:

> type Number = ExtendedDecimal P16

A decimal number type may be used in a @default@ declaration, possibly
replacing 'Double' and/or 'Integer'. For example:

> default (Integer, BasicDecimal)

== Advanced usage

Additional operations and control beyond what is provided by the standard type
classes are available through the use of "Numeric.Decimal.Arithmetic" and
"Numeric.Decimal.Operation". Advanced string conversion is also available
through "Numeric.Decimal.Conversion".
-}
