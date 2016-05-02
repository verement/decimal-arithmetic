-- -*- Haskell -*-

module Numeric.Decimal.Conversions
       ( toScientificString
       , toNumber
       ) where

import Text.ParserCombinators.ReadP (ReadP)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision (Precision)
import {-# SOURCE #-} Numeric.Decimal.Rounding (Rounding)

toScientificString :: Number p r -> ShowS
toNumber :: (Precision p, Rounding r) => ReadP (Number p r)
