
module Harness where

import Prelude hiding (exponent)

import Text.ParserCombinators.ReadP

import Numeric.Decimal
import Numeric.Decimal.Arithmetic
import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

import qualified Numeric.Decimal.Operation as Op

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


op1 :: (BasicDecimal -> Arith P9 RoundHalfUp BasicDecimal)
    -> String -> BasicDecimal
op1 op x = either exceptionResult id $ evalArith arith newContext
  where arith = op (read x)

op2 :: (BasicDecimal -> BasicDecimal -> Arith P9 RoundHalfUp BasicDecimal)
    -> String -> String -> BasicDecimal
op2 op x y = either exceptionResult id $ evalArith arith newContext
  where arith = op (read x) (read y)
