
module Harness where

import Prelude hiding (exponent)

import Text.ParserCombinators.ReadP

import Numeric.Decimal
import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

import Arbitrary

data Rep = N (Int, Coefficient, Exponent)
         | I Int
         | Q (Int, Coefficient)
         | S (Int, Coefficient)
         deriving (Eq, Show)

toRep :: GeneralDecimal -> Rep
toRep n = case n of
  Num  { sign = s, coefficient = c, exponent = e } -> N (fromEnum s, c, e)
  Inf  { sign = s }                                -> I (fromEnum s)
  QNaN { sign = s, payload = p }                   -> Q (fromEnum s, p)
  SNaN { sign = s, payload = p }                   -> S (fromEnum s, p)

fromRep :: Rep -> GeneralDecimal
fromRep r = case r of
  N (s, c, e) -> Num  { sign = toEnum s, coefficient = c, exponent = e }
  I s         -> Inf  { sign = toEnum s }
  Q (s, p)    -> QNaN { sign = toEnum s, payload = p }
  S (s, p)    -> SNaN { sign = toEnum s, payload = p }

show' :: (GeneralDecimal -> ShowS) -> GeneralDecimal -> String
show' s = ($ "") . s

read' :: ReadP GeneralDecimal -> String -> GeneralDecimal
read' p str = case [ x | (x, "") <- readP_to_S p str ] of
  [x] -> x
  _   -> error "read failed"

castDown :: (Precision p, Rounding r)
         => Number (PPlus1 (PPlus1 p)) a -> Number p r
castDown = cast
