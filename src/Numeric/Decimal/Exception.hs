
module Numeric.Decimal.Exception (
  -- * Exceptional conditions
    clamped
  , conversionSyntax
  , divisionByZero
  , divisionImpossible
  , divisionUndefined
  , inexact
  , insufficientStorage
  , invalidContext
  , invalidOperation
  , overflow
  , rounded
  , subnormal
  , underflow
  ) where

import Prelude hiding (exponent)

import Numeric.Decimal.Arithmetic
import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

-- | This occurs and signals 'Clamped' if the exponent of a result has been
-- altered in order to fit the constraints of a specific concrete
-- representation. This may occur when the exponent of a zero result would be
-- outside the bounds of a representation, or (in the IEEE 754 interchange
-- formats) when a large normal number would have an encoded exponent that
-- cannot be represented. In this latter case, the exponent is reduced to fit
-- and the corresponding number of zero digits are appended to the coefficient
-- (“fold-down”). The condition always occurs when a subnormal value rounds to
-- zero.
clamped :: Decimal p r -> Arith p r (Decimal p r)
clamped = raiseSignal Clamped

-- | This occurs and signals 'InvalidOperation' if a string is being converted
-- to a number and it does not conform to the numeric string syntax. The
-- result is @[0,qNaN]@.
conversionSyntax :: Arith p r (Decimal p r)
conversionSyntax = raiseSignal InvalidOperation qNaN

-- | This occurs and signals 'DivisionByZero' if division of a finite number
-- by zero was attempted (during a 'Numeric.Decimal.Operation.divideInteger'
-- or 'Numeric.Decimal.Operation.divide' operation, or a
-- 'Numeric.Decimal.Operation.power' operation with negative right-hand
-- operand), and the dividend was not zero.
--
-- The result of the operation is @[@/sign/@,inf]@, where /sign/ is the
-- exclusive or of the signs of the operands for divide, or is 1 for an odd
-- power of −0, for power.
divisionByZero :: Decimal p r -> Arith p r (Decimal p r)
divisionByZero = raiseSignal DivisionByZero

-- | This occurs and signals 'InvalidOperation' if the integer result of a
-- 'Numeric.Decimal.Operation.divideInteger' or
-- 'Numeric.Decimal.Operation.remainder' operation had too many digits (would
-- be longer than /precision/). The result is @[0,qNaN]@.
divisionImpossible :: Arith p r (Decimal p r)
divisionImpossible = raiseSignal InvalidOperation qNaN

-- | This occurs and signals 'InvalidOperation' if division by zero was
-- attempted (during a 'Numeric.Decimal.Operation.divideInteger',
-- 'Numeric.Decimal.Operation.divide', or
-- 'Numeric.Decimal.Operation.remainder' operation), and the dividend is also
-- zero. The result is @[0,qNaN]@.
divisionUndefined :: Arith p r (Decimal p r)
divisionUndefined = raiseSignal InvalidOperation qNaN

-- | This occurs and signals 'Inexact' whenever the result of an operation is
-- not exact (that is, it needed to be rounded and any discarded digits were
-- non-zero), or if an overflow or underflow condition occurs. The result in
-- all cases is unchanged.
--
-- The 'Inexact' signal may be tested (or trapped) to determine if a given
-- operation (or sequence of operations) was inexact.
inexact :: Decimal p r -> Arith p r (Decimal p r)
inexact = raiseSignal Inexact

-- | For many implementations, storage is needed for calculations and
-- intermediate results, and on occasion an arithmetic operation may fail due
-- to lack of storage. This is considered an operating environment error,
-- which can be either be handled as appropriate for the environment, or
-- treated as an Invalid operation condition. The result is @[0,qNaN]@.
insufficientStorage :: Arith p r (Decimal p r)
insufficientStorage = invalidOperation qNaN

-- | This occurs and signals 'InvalidOperation' if an invalid context was
-- detected during an operation. This can occur if contexts are not checked on
-- creation and either the /precision/ exceeds the capability of the
-- underlying concrete representation or an unknown or unsupported /rounding/
-- was specified. These aspects of the context need only be checked when the
-- values are required to be used. The result is @[0,qNaN]@.
invalidContext :: Arith p r (Decimal p r)
invalidContext = raiseSignal InvalidOperation qNaN

-- | This occurs and signals 'InvalidOperation' if:
--
-- * an operand to an operation is @[s,sNaN]@ or @[s,sNaN,d]@ (any /signaling/
-- NaN)
--
-- * an attempt is made to add @[0,inf]@ to @[1,inf]@ during an addition or
-- subtraction operation
--
-- * an attempt is made to multiply 0 by @[0,inf]@ or @[1,inf]@
--
-- * an attempt is made to divide either @[0,inf]@ or @[1,inf]@ by either
-- @[0,inf]@ or @[1,inf]@
--
-- * the divisor for a remainder operation is zero
--
-- * the dividend for a remainder operation is either @[0,inf]@ or @[1,inf]@
--
-- * either operand of the 'Numeric.Decimal.Operation.quantize' operation is
-- infinite, or the result of a 'Numeric.Decimal.Operation.quantize' operation
-- would require greater precision than is available
--
-- * the operand of the 'Numeric.Decimal.Operation.ln' or the
-- 'Numeric.Decimal.Operation.log10' operation is less than zero
--
-- * the operand of the 'Numeric.Decimal.Operation.squareRoot' operation has a
-- /sign/ of 1 and a non-zero /coefficient/
--
-- * both operands of the 'Numeric.Decimal.Operation.power' operation are
-- zero, or if the left-hand operand is less than zero and the right-hand
-- operand does not have an integral value or is infinite
--
-- * an operand is invalid; for example, certain values of concrete
-- representations may not correspond to numbers — an implementation is
-- permitted (but is not required) to detect these invalid values and raise
-- this condition.
--
-- The result of the operation after any of these invalid operations is
-- @[0,qNaN]@ except when the cause is a signaling NaN, in which case the
-- result is @[s,qNaN]@ or @[s,qNaN,d]@ where the sign and diagnostic are
-- copied from the signaling NaN.
invalidOperation :: Decimal a b -> Arith p r (Decimal p r)
invalidOperation n = raiseSignal InvalidOperation $ case n of
  NaN { signaling = True } -> n { signaling = False }
  _                        -> qNaN

-- | This occurs and signals 'Overflow' if the /adjusted exponent/ of a result
-- (from a conversion or from an operation that is not an attempt to divide by
-- zero), after rounding, would be greater than the largest value that can be
-- handled by the implementation (the value E/max/).
--
-- The result depends on the rounding mode:
--
-- * For 'RoundHalfUp' and 'RoundHalfEven' (and for 'RoundHalfDown' and
-- 'RoundUp', if implemented), the result of the operation is [sign,@inf@],
-- where /sign/ is the sign of the intermediate result.
--
-- * For 'RoundDown', (and 'Round05Up', if implemented), the result is the
-- largest finite number that can be represented in the current /precision/,
-- with the sign of the intermediate result.
--
-- * For 'RoundCeiling', the result is the same as for 'RoundDown' if the sign
-- of the intermediate result is 1, or is @[0,inf]@ otherwise.
--
-- * For 'RoundFloor', the result is the same as for 'RoundDown' if the sign
-- of the intermediate result is 0, or is @[1,inf]@ otherwise.
--
-- In all cases, 'inexact' and 'rounded' will also be raised.
--
-- Note: IEEE 854 §7.3 requires that the result delivered to a trap handler be
-- different, depending on whether the overflow was the result of a conversion
-- or of an arithmetic operation. This specification deviates from IEEE 854 in
-- this respect; however, an implementation could comply with IEEE 854 by
-- providing a separate mechanism for the special result to a trap
-- handler. IEEE 754 has no such requirement.
overflow :: (Precision p, Rounding r) => Decimal p r -> Arith p r (Decimal p r)
overflow ir = result >>= raiseSignal Overflow >>= inexact >>= rounded

  where result :: (Precision p, Rounding r) => Arith p r (Decimal p r)
        result = getRounding >>= \r -> case r of
          RoundHalfUp   -> signedInfinity
          RoundHalfEven -> signedInfinity
          RoundHalfDown -> signedInfinity
          RoundUp       -> signedInfinity

          RoundDown     -> largestFinite
          Round05Up     -> largestFinite

          RoundCeiling  -> case sign ir of
            Neg -> largestFinite
            Pos -> return infinity

          RoundFloor    -> case sign ir of
            Pos -> largestFinite
            Neg -> return infinity { sign = Neg }

        signedInfinity :: Arith p r (Decimal p r)
        signedInfinity = return infinity { sign = sign ir }

        largestFinite :: Precision p => Arith p r (Decimal p r)
        largestFinite = getPrecision >>= \p ->
          let x = Num { sign = sign ir
                      , coefficient = undefined -- 10^p - 1
                      , exponent = undefined -- eMax x
                      }
          in return x

-- | This occurs and signals 'Rounded' whenever the result of an operation is
-- rounded (that is, some zero or non-zero digits were discarded from the
-- coefficient), or if an overflow or underflow condition occurs. The result
-- in all cases is unchanged.
--
-- The 'Rounded' signal may be tested (or trapped) to determine if a given
-- operation (or sequence of operations) caused a loss of precision.
rounded :: Decimal p r -> Arith p r (Decimal p r)
rounded = raiseSignal Rounded

-- | This occurs and signals 'Subnormal' whenever the result of a conversion
-- or operation is subnormal (that is, its adjusted exponent is less than
-- E/min/, before any rounding). The result in all cases is unchanged.
--
-- The 'Subnormal' signal may be tested (or trapped) to determine if a given
-- or operation (or sequence of operations) yielded a subnormal result.
subnormal :: Decimal p r -> Arith p r (Decimal p r)
subnormal = raiseSignal Subnormal

-- | This occurs and signals 'Underflow' if a result is inexact and the
-- /adjusted exponent/ of the result would be smaller (more negative) than the
-- smallest value that can be handled by the implementation (the value
-- E/min/). That is, the result is both inexact and subnormal.
--
-- The result after an underflow will be a subnormal number rounded, if
-- necessary, so that its exponent is not less than E/tiny/. This may result
-- in 0 with the sign of the intermediate result and an exponent of E/tiny/.
--
-- In all cases, 'inexact', 'rounded', and 'subnormal' will also be raised.
--
-- Note: IEEE 854 §7.4 requires that the result delivered to a trap handler be
-- different, depending on whether the underflow was the result of a
-- conversion or of an arithmetic operation. This specification deviates from
-- IEEE 854 in this respect; however, an implementation could comply with IEEE
-- 854 by providing a separate mechanism for the result to a trap
-- handler. IEEE 754 has no such requirement.
underflow :: Decimal p r -> Arith p r (Decimal p r)
underflow x = undefined >>= raiseSignal Underflow >>=
  subnormal >>= inexact >>= rounded
