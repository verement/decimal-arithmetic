-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Number
       ( Sign(..)
       , Coefficient
       , Exponent
       , Decimal(..)
       , numDigits
       ) where

import Numeric.Natural (Natural)

data Sign = Pos | Neg

type Coefficient = Natural
type Exponent    = Integer
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

numDigits :: Coefficient -> Int
