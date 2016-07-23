
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

exceptionError :: Exception p r -> a
exceptionError = error . show . exceptionSignal

type BasicArith = Arith P9 RoundHalfUp

op0 :: BasicArith a -> a
op0 op = either exceptionError id $ evalArith op newContext

op1 :: (BasicDecimal -> BasicArith a) -> String -> a
op1 op x = either exceptionError id $ evalArith arith newContext
  where arith = op (read x)

op2 :: (BasicDecimal -> BasicDecimal -> BasicArith a) -> String -> String -> a
op2 op x y = either exceptionError id $ evalArith arith newContext
  where arith = read x `op` read y

op3 :: (BasicDecimal -> BasicDecimal -> BasicDecimal -> BasicArith a)
    -> String -> String -> String -> a
op3 op x y z = either exceptionError id $ evalArith arith newContext
  where arith = op (read x) (read y) (read z)
