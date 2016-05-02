-- -*- Haskell -*-

{-# LANGUAGE RoleAnnotations #-}

module Numeric.Decimal.Number
       ( Number
       ) where

type role Number phantom phantom
data Number p r
