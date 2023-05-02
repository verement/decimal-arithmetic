-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Arithmetic
       ( Arith
       , newContext
       , evalArith
       , getPrecision
       , Signal(..)
       , raiseSignal
       , exceptionResult
       ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (State)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision (Precision)

data Context p r
newContext :: Context p r

newtype Arith p r a = Arith (ExceptT (Exception p r) (State (Context p r)) a)
instance Functor (Arith p r)
instance Applicative (Arith p r)
instance Monad (Arith p r)
evalArith :: Arith p r a -> Context p r -> Either (Exception p r) a
getPrecision :: Precision p => Arith p r (Maybe Int)

data Signal
  = Clamped
  | DivisionByZero
  | Inexact
  | InvalidOperation
  | Overflow
  | Rounded
  | Subnormal
  | Underflow
  | GasExceeded
raiseSignal :: Signal -> Decimal p r -> Arith p r (Decimal p r)

type role Exception phantom phantom
data Exception p r
exceptionResult :: Exception p r -> Decimal p r
