-- -*- Haskell -*-

module Numeric.Decimal.Conversion
       ( toScientificString
       , toNumber
       ) where

import Text.ParserCombinators.ReadP (ReadP)

import {-# SOURCE #-} Numeric.Decimal.Number
import                Numeric.Decimal.Precision

toScientificString :: Number p r -> ShowS
toNumber :: ReadP (Number PInfinite r)
