
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | It is not usually necessary to import this module unless you want to use
-- the arithmetic operations from "Numeric.Decimal.Operation" or you need
-- precise control over the handling of exceptional conditions in an
-- arithmetic computation.

module Numeric.Decimal.Arithmetic
       ( -- *Decimal arithmetic
         -- $decimal-arithmetic

         -- ** Context
         Context
       , newContext
       , flags

         -- *** Default contexts
         -- $default-contexts
       , basicDefaultContext
       , extendedDefaultContext

         -- ** The Arith monad
       , Arith
       , runArith
       , evalArith

         -- * Exceptional conditions
         -- $exceptional-conditions
       , Exception
       , exceptionSignal
       , exceptionResult

         -- ** Signals
       , Signal(..)
       , Signals
       , signal
       , signals
       , testSignal

       , raiseSignal
       , clearFlags

         -- ** Traps
       , TrapHandler
       , setTrapHandler
       , enableTraps
       , disableTraps
       , withTraps
       , withoutTraps
       ) where

import Control.Monad.Except (MonadError(throwError, catchError),
                             ExceptT, runExceptT)
import Control.Monad.State (MonadState(get, put), modify,
                            State, runState, evalState)
import Data.Bits (bit, complement, testBit, (.&.), (.|.))
import Data.Monoid ((<>))

import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

-- $decimal-arithmetic
--
-- Decimal arithmetic is performed within a context that maintains state to
-- handle exceptional conditions such as underflow, rounding, or division by
-- zero (cf. 'Signal'). The 'Arith' monad provides a means to evaluate an
-- arithmetic computation and manipulate its 'Context'.

-- | A context for decimal arithmetic, carrying signal flags, trap enabler
-- state, and a trap handler, parameterized by precision @p@ and rounding
-- algorithm @r@
data Context p r =
  Context { flags        :: Signals
                            -- ^ The current signal flags of the context
          , trapEnablers :: Signals
                            -- ^ The current trap enablers of the context
          , trapHandler  :: TrapHandler p r
                            -- ^ The trap handler function for the context
          }

-- | Return a new context with all signal flags cleared, all traps disabled,
-- and a trap handler that calls 'throwError' on all exceptional conditions
-- (when enabled).
newContext :: Context p r
newContext = Context { flags        = mempty
                     , trapEnablers = mempty
                     , trapHandler  = throwError
                     }

-- $default-contexts
--
-- The /General Decimal Arithmetic/ specification defines optional default
-- contexts, which define suitable settings for basic arithmetic and for the
-- extended arithmetic defined by IEEE 854 and IEEE 754.

-- | Return a new context with all signal flags cleared, all traps enabled
-- except for 'Inexact', 'Rounded', and 'Subnormal', using a precision of 9
-- significant decimal digits, and rounding half up.
basicDefaultContext :: Context P9 RoundHalfUp
basicDefaultContext = newContext { trapEnablers = traps }
  where traps = signals $ filter (`notElem` [Inexact, Rounded, Subnormal])
          [minBound..maxBound]

-- | Return a new context with all signal flags cleared, all traps disabled,
-- using selectable precision, and rounding half even.
extendedDefaultContext :: Context p RoundHalfEven
extendedDefaultContext = newContext

-- | A representation of an exceptional condition
data Exception p r =
  Exception { exceptionSignal :: Signal
                                 -- ^ The signal raised by the exceptional
                                 -- condition
            , exceptionResult :: Number p r
                                 -- ^ The defined result for the exceptional
                                 -- condition
            }
  deriving Show

-- | A decimal arithmetic monad parameterized by the precision @p@ and
-- rounding algorithm @r@
newtype Arith p r a = Arith (ExceptT (Exception p r)
                             (State (Context p r)) a)

instance Functor (Arith p r) where
  fmap f (Arith s) = Arith (fmap f s)

instance Applicative (Arith p r) where
  pure = Arith . pure
  Arith f <*> Arith e = Arith (f <*> e)

instance Monad (Arith p r) where
  Arith e >>= f = Arith (e >>= g)
    where g x = let Arith t = f x in t

instance MonadError (Exception p r) (Arith p r) where
  throwError e = Arith (throwError e)
  catchError (Arith e) f = Arith (catchError e g)
    where g x = let Arith t = f x in t

instance MonadState (Context p r) (Arith p r) where
  get = Arith   get
  put = Arith . put

-- | Evaluate an arithmetic computation in the given context and return the
-- final value (or exception) and resulting context.
runArith :: Arith p r a -> Context p r
         -> (Either (Exception p r) a, Context p r)
runArith (Arith e) = runState (runExceptT e)

-- | Evaluate an arithmetic computation in the given context and return the
-- final value or exception, discarding the resulting context.
evalArith :: Arith p r a -> Context p r -> Either (Exception p r) a
evalArith (Arith e) = evalState (runExceptT e)

-- $exceptional-conditions
--
-- Exceptional conditions are grouped into signals, which can be controlled
-- individually. A 'Context' contains a flag and a trap enabler (i.e. enabled
-- or disabled) for each 'Signal'.

data Signal
  = Clamped
    -- ^ Raised when the exponent of a result has been altered or constrained
    -- in order to fit the constraints of a specific concrete representation
  | DivisionByZero
    -- ^ Raised when a non-zero dividend is divided by zero
  | Inexact
    -- ^ Raised when a result is not exact (one or more non-zero coefficient
    -- digits were discarded during rounding)
  | InvalidOperation
    -- ^ Raised when a result would be undefined or impossible
  | Overflow
    -- ^ Raised when the exponent of a result is too large to be represented
  | Rounded
    -- ^ Raised when a result has been rounded (that is, some zero or non-zero
    -- coefficient digits were discarded)
  | Subnormal
    -- ^ Raised when a result is subnormal (its adjusted exponent is less than
    -- E/min/), before any rounding
  | Underflow
    -- ^ Raised when a result is both subnormal and inexact
  deriving (Eq, Enum, Bounded, Show)

-- | A group of signals can be manipulated as a set.
newtype Signals = Signals Int
                deriving Eq

instance Show Signals where
  showsPrec d sigs = showParen (d > 10) $
    showString "signals " . showsPrec 11 (signalList sigs)

instance Monoid Signals where
  mempty = Signals 0
  Signals x `mappend` Signals y = Signals (x .|. y)

-- | Create a set of signals from a singleton.
signal :: Signal -> Signals
signal = Signals . bit . fromEnum

-- | Create a set of signals from a list.
signals :: [Signal] -> Signals
signals = mconcat . map signal

-- | Enumerate the given set of signals.
signalList :: Signals -> [Signal]
signalList sigs = filter (`testSignal` sigs) [minBound..maxBound]

-- | Remove the first set of signals from the second.
unsignal :: Signals -> Signals -> Signals
unsignal (Signals u) (Signals ss) = Signals (ss .&. complement u)

-- | Determine whether a signal is a member of a set.
testSignal :: Signal -> Signals -> Bool
testSignal sig (Signals ss) = testBit ss (fromEnum sig)

-- | Set the given signal flag in the context of the current arithmetic
-- computation, and call the trap handler if the trap for this signal is
-- currently enabled.
raiseSignal :: Signal -> Number p r -> Arith p r (Number p r)
raiseSignal sig n = do
  ctx <- get
  let ctx' = ctx { flags = flags ctx <> signal sig }
  put ctx'
  if testSignal sig (trapEnablers ctx')
    then trapHandler ctx' (Exception sig n)
    else return n

-- | Clear the given signal flags from the context of the current arithmetic
-- computation.
clearFlags :: Signals -> Arith p r ()
clearFlags sigs = modify $ \ctx -> ctx { flags = unsignal sigs (flags ctx) }

-- | A trap handler function may return a substitute result for the operation
-- that caused the exceptional condition, or it may call 'throwError' to abort
-- the arithmetic computation (or pass control to an appropriate 'catchError'
-- handler).
--
-- Care should be taken not to cause additional traps that might result in an
-- infinite loop.
type TrapHandler p r = Exception p r -> Arith p r (Number p r)

-- | Set the trap handler function for the context of the current arithmetic
-- computation. The handler will be called whenever a signal is raised for
-- which the corresponding trap is also enabled.
setTrapHandler :: TrapHandler p r -> Arith p r ()
setTrapHandler handler = modify $ \ctx -> ctx { trapHandler = handler }

-- | Enable traps for all of the given signals.
enableTraps :: Signals -> Arith p r ()
enableTraps sigs = modify $ \ctx ->
  ctx { trapEnablers = trapEnablers ctx <> sigs }

-- | Disable traps for all of the given signals.
disableTraps :: Signals -> Arith p r ()
disableTraps sigs = modify $ \ctx ->
  ctx { trapEnablers = unsignal sigs (trapEnablers ctx) }

-- | Perform an arithmetic computation with a locally modified set of trap
-- enablers. The former trap enablers are restored after the computation is
-- completed.
localTraps :: (Signals -> Signals) -> Arith p r a -> Arith p r a
localTraps f comp = do
  ctx <- get
  let enablers = trapEnablers ctx
  put ctx { trapEnablers = f enablers }
  r <- comp
  put ctx { trapEnablers = enablers }
  return r

-- | Enable traps for all of the given signals for the duration of the passed
-- arithmetic computation. The set of trap enablers is restored after the
-- computation is completed.
withTraps :: Signals -> Arith p r a -> Arith p r a
withTraps sigs = localTraps (<> sigs)

-- | Disable traps for all of the given signals for the duration of the passed
-- arithmetic computation. The set of trap enablers is restored after the
-- computation is completed.
withoutTraps :: Signals -> Arith p r a -> Arith p r a
withoutTraps sigs = localTraps (unsignal sigs)
