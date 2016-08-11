-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Number
       ( Sign(..)
       , Decimal(..)
       , Coefficient
       , numDigits
       ) where

import Numeric.Natural (Natural)

import Numeric.Decimal.Precision

data Sign = Pos | Neg
instance Eq Sign

type Coefficient = Natural
type Exponent    = Int
type Payload     = Coefficient

type role Decimal phantom phantom
data Decimal p r
  = Num { sign        :: Sign
        , coefficient :: Coefficient
        , exponent    :: Exponent
        }
  | Inf { sign        :: Sign
        }
  | NaN { sign        :: Sign
        , signaling   :: Bool
        , payload     :: Payload
        }

instance Precision p => Precision (Decimal p r)

numDigits :: Coefficient -> Int
