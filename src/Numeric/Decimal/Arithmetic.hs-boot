-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Arithmetic
       ( Arith
       , newContext
       , evalArith
       , Signal(..)
       , raiseSignal
       , exceptionResult
       ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)

import {-# SOURCE #-} Numeric.Decimal.Number

--instance Precision p => Precision (Arith p r a)

data Context p r
newContext :: Context p r

newtype Arith p r a = Arith (ExceptT (Exception p r) (State (Context p r)) a)
instance Functor (Arith p r)
instance Applicative (Arith p r)
instance Monad (Arith p r)
evalArith :: Arith p r a -> Context p r -> Either (Exception p r) a

data Signal
  = Clamped
  | DivisionByZero
  | Inexact
  | InvalidOperation
  | Overflow
  | Rounded
  | Subnormal
  | Underflow
raiseSignal :: Signal -> Number p r -> Arith p r (Number p r)

type role Exception phantom phantom
data Exception p r
exceptionResult :: Exception p r -> Number p r
