-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Number
       ( Sign(..)
       , Number(..)
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

type role Number phantom phantom
data Number p r
  = Num  { sign        :: Sign
         , coefficient :: Coefficient
         , exponent    :: Exponent
         }
  | Inf  { sign        :: Sign
         }
  | QNaN { sign        :: Sign
         , payload     :: Payload
         }
  | SNaN { sign        :: Sign
         , payload     :: Payload
         }

instance Precision p => Precision (Number p r)

numDigits :: Coefficient -> Int
