{-|
Module      : Numeric.Decimal
Description : General arbitrary-precision decimal floating-point number type
Copyright   : © 2016–2017 Robert Leslie
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

Some decimal numbers also support encoding and decoding specific IEEE 754
interchange formats via a 'Data.Binary.Binary' instance.
-}
module Numeric.Decimal
       ( -- * Usage
         -- $usage

         -- ** Advanced usage
         -- $advanced-usage

         -- * Arbitrary-precision decimal numbers
         Decimal
       , BasicDecimal
       , ExtendedDecimal
       , GeneralDecimal

         -- ** Number types with defined encodings
         -- $encodings
       , Decimal32
       , Decimal64
       , Decimal128

         -- ** Precision types
       , Precision
       , FinitePrecision

       , P1 , P2 , P3 , P4 , P5 , P6 , P7 , P8 , P9 , P10
       , P11, P12, P13, P14, P15, P16, P17, P18, P19, P20
       , P21, P22, P23, P24, P25, P26, P27, P28, P29, P30
       , P31, P32, P33, P34, P35, P36, P37, P38, P39, P40
       , P41, P42, P43, P44, P45, P46, P47, P48, P49, P50

       , P75, P100, P150, P200, P250, P300, P400, P500, P1000, P2000

       , PPlus1, PTimes2

       , PInfinite

       , Pdecimal32
       , Pdecimal64
       , Pdecimal128

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
that rounds half even. For example, @'ExtendedDecimal' 'P15'@ is a number type
with 15 decimal digits of precision. There is a range of ready-made precisions
available, including 'P1' through 'P50' on up to 'P2000'.

* 'GeneralDecimal' is a number type with infinite precision. Note that not all
operations support numbers with infinite precision.

* 'Decimal32', 'Decimal64', and 'Decimal128' are specialized number types with
'Data.Binary.Binary' instances that implement the /decimal32/, /decimal64/,
and /decimal128/ interchange format encodings described in IEEE 754-2008.
These types have precisions of 7, 16, and 34 decimal digits, respectively, and
round half even.

* The most versatile 'Decimal' type constructor is parameterized by both a
precision and a rounding algorithm. For example, @'Decimal' 'P20' 'RoundDown'@
is a number type with 20 decimal digits of precision that rounds down
(truncates). Several 'Rounding' algorithms are available to choose from.

A decimal number type may be used in a @default@ declaration, for example
replacing 'Double':

> default (Integer, BasicDecimal)
-}

{- $advanced-usage

Additional operations and control beyond what is provided by the standard type
classes are available through the use of "Numeric.Decimal.Arithmetic" and
"Numeric.Decimal.Operation". Advanced string conversion is also available
through "Numeric.Decimal.Conversion".

Arbitrary precisions can be constructed through type application of 'PPlus1'
and/or 'PTimes2' to any existing precision.

It is possible to create arbitrary width interchange format encodings with the
help of "Numeric.Decimal.Encoding".
-}

{- $encodings

These decimal number types have a 'Data.Binary.Binary' instance that
implements a specific interchange format encoding described in IEEE
754-2008. See "Numeric.Decimal.Encoding" for further details, including the
ability to create additional formats of arbitrary width.

Alternative rounding algorithms can be used through the more general 'Decimal'
type constructor and the special precision types 'Pdecimal32', 'Pdecimal64',
or 'Pdecimal128', e.g. @'Decimal' 'Pdecimal64' 'RoundCeiling'@.
-}
