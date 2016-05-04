
module Numeric.Decimal.Number
       ( Sign(..)
       , negateSign
       , xorSigns

       , Coefficient
       , numDigits

       , Exponent
       , Payload

       , Number(..)
       , zero
       , one
       , negativeOne
       , infinity
       , qNaN
       , sNaN

       , flipSign
       , cast
       , excessDigits

       , isPositive
       , isNegative
       , isFinite
       , isZero
       , isNormal
       , isSubnormal

       , Context(..)
       , TrapHandler
       , defaultContext
       , mergeContexts

       , Signal(..)
       , raiseSignal
       ) where

import Prelude hiding (exponent, round)

import Data.Bits (bit, complement, testBit, (.&.), (.|.))
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Monoid ((<>))
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)

import {-# SOURCE #-} Numeric.Decimal.Conversion
import                Numeric.Decimal.Precision
import {-# SOURCE #-} Numeric.Decimal.Rounding

import {-# SOURCE #-} qualified Numeric.Decimal.Operation as Op

import qualified GHC.Real

data Sign = Pos | Neg
          deriving (Eq, Enum, Show)

negateSign :: Sign -> Sign
negateSign Pos = Neg
negateSign Neg = Pos

xorSigns :: Sign -> Sign -> Sign
xorSigns Pos Pos = Pos
xorSigns Pos Neg = Neg
xorSigns Neg Pos = Neg
xorSigns Neg Neg = Pos

signFactor :: Num a => Sign -> a
signFactor Pos =  1
signFactor Neg = -1

signFunc :: Num a => Sign -> a -> a
signFunc Pos = id
signFunc Neg = negate

type Coefficient = Natural
type Exponent = Integer

type Payload = Coefficient

-- | A decimal floating point number with selectable precision and rounding
-- algorithm
data Number p r
  = Num  { context     :: Context p r
         , sign        :: Sign
         , coefficient :: Coefficient
         , exponent    :: Exponent
         }
  | Inf  { context     :: Context p r
         , sign        :: Sign
         }
  | QNaN { context     :: Context p r
         , sign        :: Sign
         , payload     :: Payload
         }
  | SNaN { context     :: Context p r
         , sign        :: Sign
         , payload     :: Payload
         }

instance Precision p => Precision (Number p r) where
  precision = precision . numberPrecision
    where numberPrecision :: Number p r -> p
          numberPrecision = undefined

instance Show (Number p r) where
  showsPrec d n = showParen (d > 0 && isNegative n) $ toScientificString n

instance (Precision p, Rounding r) => Read (Number p r) where
  readsPrec _ = readParen False $ readP_to_S toNumber . dropWhile isSpace

instance (Precision p, Rounding r) => Eq (Number p r) where
  x == y = case x `Op.compare` y of
    Num { coefficient = 0 } -> True
    _                       -> False

instance (Precision p, Rounding r) => Ord (Number p r) where
  x `compare` y = case x `Op.compare` y of
    Num { coefficient = 0 } -> EQ
    Num { sign = Neg      } -> LT
    Num { sign = Pos      } -> GT
    _                       -> GT  -- match Prelude behavior for NaN

  x < y = case x `Op.compare` y of
    Num { sign = Neg      } -> True
    _                       -> False

  x <= y = case x `Op.compare` y of
    Num { sign = Neg      } -> True
    Num { coefficient = 0 } -> True
    _                       -> False

  x > y = case x `Op.compare` y of
    Num { coefficient = 0 } -> False
    Num { sign = Pos      } -> True
    _                       -> False

  x >= y = case x `Op.compare` y of
    Num { sign = Pos      } -> True
    _                       -> False

  max nan@SNaN{} _ = nan
  max _ nan@SNaN{} = nan
  max nan@QNaN{} _ = nan
  max _ nan@QNaN{} = nan
  max x y
    | x >= y    = x
    | otherwise = y

  min nan@SNaN{} _ = nan
  min _ nan@SNaN{} = nan
  min nan@QNaN{} _ = nan
  min _ nan@QNaN{} = nan
  min x y
    | x < y     = x
    | otherwise = y

instance (FinitePrecision p, Rounding r) => Enum (Number p r) where
  toEnum = fromIntegral
  fromEnum = truncate

instance (Precision p, Rounding r) => Num (Number p r) where
  (+)    = Op.add
  (-)    = Op.subtract
  (*)    = Op.multiply
  negate = Op.minus
  abs    = Op.abs

  signum n = case n of
    Num { coefficient = 0 } -> zero
    Num { sign = s        } -> one { sign = s }
    Inf { sign = s        } -> one { sign = s }
    _                       -> n

  fromInteger x = Num { context     = defaultContext
                      , sign        = sx
                      , coefficient = fromInteger (abs x)
                      , exponent    = 0
                      }
    where sx = case signum x of
            -1 -> Neg
            _  -> Pos

instance (Precision p, Rounding r) => Real (Number p r) where
  toRational Num { sign = s, coefficient = c, exponent = e }
    | e >= 0    = fromInteger (signFactor s * fromIntegral c * 10^e)
    | otherwise = (signFactor s * fromIntegral c) % 10^(-e)
  toRational n = signFunc (sign n) $ case n of
    Inf{} -> GHC.Real.infinity
    _     -> GHC.Real.notANumber

instance (FinitePrecision p, Rounding r) => Fractional (Number p r) where
  (/) = Op.divide
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (FinitePrecision p, Rounding r) => RealFrac (Number p r) where
  properFraction x@Num { sign = s, coefficient = c, exponent = e }
    | e < 0     = (n, f)
    | otherwise = (signFactor s * fromIntegral c * 10^e, zero)
    where n = signFactor s * fromIntegral q
          f = x { coefficient = r, exponent = -(fromIntegral $ numDigits r) }
          (q, r) = c `quotRem` (10^(-e))
  properFraction nan = (0, nan)

-- | A 'Number' representing the value zero
zero :: Number p r
zero = Num { context     = defaultContext
           , sign        = Pos
           , coefficient = 0
           , exponent    = 0
           }

-- | A 'Number' representing the value one
one :: Number p r
one = zero { coefficient = 1 }

-- | A 'Number' representing the value negative one
negativeOne :: Number p r
negativeOne = one { sign = Neg }

-- | A 'Number' representing the value positive infinity
infinity :: Number p r
infinity = Inf { context = defaultContext, sign = Pos }

-- | A 'Number' representing undefined results
qNaN :: Number p r
qNaN = QNaN { context = defaultContext, sign = Pos, payload = 0 }

-- | A signaling 'Number' representing undefined results
sNaN :: Number p r
sNaN = SNaN { context = defaultContext, sign = Pos, payload = 0 }

-- | Negate the given 'Number' by directly flipping its sign.
flipSign :: Number p r -> Number p r
flipSign n = n { sign = negateSign (sign n) }

-- | Cast a 'Number' to another precision and/or rounding algorithm,
-- immediately rounding if necessary to the new precision using the new
-- algorithm.
cast :: (Precision p, Rounding r) => Number a b -> Number p r
cast = round . coerce

numDigits :: Coefficient -> Int
numDigits x
  | x <         10 = 1
  | x <        100 = 2
  | x <       1000 = 3
  | x <      10000 = 4
  | x <     100000 = 5
  | x <    1000000 = 6
  | x <   10000000 = 7
  | x <  100000000 = 8
  | x < 1000000000 = 9
  | otherwise      = 9 + numDigits (x `quot` 1000000000)

excessDigits :: Precision p => Number p r -> Maybe Int
excessDigits x@Num { coefficient = c } = precision x >>= excess
  where excess p
          | d > p     = Just (d - p)
          | otherwise = Nothing
          where d = numDigits c
excessDigits _ = Nothing

maxCoefficient :: Precision p => p -> Maybe Coefficient
maxCoefficient p = (\d -> 10 ^ d - 1) <$> precision p

-- | Is the sign of the given 'Number' positive?
isPositive :: Number p r -> Bool
isPositive n = case sign n of
  Pos -> True
  Neg -> False

-- | Is the sign of the given 'Number' negative?
isNegative :: Number p r -> Bool
isNegative n = case sign n of
  Neg -> True
  Pos -> False

-- | Does the given 'Number' represent a finite value?
isFinite :: Number p r -> Bool
isFinite Num{} = True
isFinite _     = False

-- | Does the given 'Number' represent the value zero?
isZero :: Number p r -> Bool
isZero Num { coefficient = 0 } = True
isZero _                       = False

-- | Is the given 'Number' normal?
isNormal :: Precision p => Number p r -> Bool
isNormal n
  | isFinite n && not (isZero n) &&
    maybe True (adjustedExponent n >=) (eMin n) = True
  | otherwise                                   = False

-- | Is the given 'Number' subnormal?
isSubnormal :: Precision p => Number p r -> Bool
isSubnormal n
  | isFinite n && not (isZero n) &&
    maybe False (adjustedExponent n <) (eMin n) = True
  | otherwise                                   = False

-- | Upper limit on the absolute value of the exponent
eLimit :: Precision p => Number p r -> Maybe Exponent
eLimit = eMax

-- | Minimum value of the adjusted exponent
eMin :: Precision p => Number p r -> Maybe Exponent
eMin n = (1 -) <$> eMax n

-- | Maximum value of the adjusted exponent
eMax :: Precision p => Number p r -> Maybe Exponent
eMax n = subtract 1 . (10 ^) . numDigits <$> base
  where mlength = precision n                    :: Maybe Int
        base = (10 *) . fromIntegral <$> mlength :: Maybe Natural

-- | Minimum value of the exponent for subnormal results
eTiny :: Precision p => Number p r -> Maybe Exponent
eTiny n = (-) <$> eMin n <*> (fromIntegral . subtract 1 <$> precision n)

-- | Range of permissible exponent values
eRange :: Precision p => Number p r -> Maybe (Exponent, Exponent)
eRange n@Num { coefficient = c } = range <$> eLimit n
  where range :: Exponent -> (Exponent, Exponent)
        range lim = (-lim - clm1 + 1, lim - clm1)
        clength = numDigits c             :: Int
        clm1 = fromIntegral (clength - 1) :: Exponent
eRange _ = Nothing

adjustedExponent :: Number p r -> Exponent
adjustedExponent Num { coefficient = c, exponent = e } =
  e + fromIntegral (clength - 1)
  where clength = numDigits c :: Int
adjustedExponent _ = error "adjustedExponent: not a finite number"

type TrapHandler p r = Signal -> Number p r -> Number p r

data Context p r = Context { signalFlags :: Signals
                           , trapHandler :: TrapHandler p r
                           }

instance Precision p => Precision (Context p r) where
  precision = precision . contextPrecision
    where contextPrecision :: Context p r -> p
          contextPrecision = undefined

defaultContext :: Context p r
defaultContext = Context mempty (const id)

setSignal :: Signal -> Context p r -> Context p r
setSignal sig cxt = cxt { signalFlags = signalFlags cxt <> signal sig }

modifyContext :: (Context p r -> Context p r) -> Number p r -> Number p r
modifyContext f n = n { context = f (context n) }

mergeContexts :: Context p r -> Context p r -> Context p r
mergeContexts cxt1 cxt2 =
  cxt1 { signalFlags = signalFlags cxt1 <> signalFlags cxt2 }

data Signal
  = Clamped
  | DivisionByZero
  | Inexact
  | InvalidOperation
  | Overflow
  | Rounded
  | Subnormal
  | Underflow
  deriving (Enum, Bounded, Show)

newtype Signals = Signals Int

instance Show Signals where
  showsPrec d sigs = showParen (d > 10) $
    showString "signals " . showsPrec 11 (signalList sigs)

instance Monoid Signals where
  mempty = Signals 0
  Signals x `mappend` Signals y = Signals (x .|. y)

signal :: Signal -> Signals
signal = Signals . bit . fromEnum

unsignal :: Signal -> Signals -> Signals
unsignal sig (Signals ss) = Signals $ ss .&. complement (bit $ fromEnum sig)

signals :: [Signal] -> Signals
signals = foldr (\s n -> signal s <> n) mempty

signalList :: Signals -> [Signal]
signalList sigs = filter (testSignal sigs) [minBound..maxBound]

testSignal :: Signals -> Signal -> Bool
testSignal (Signals ss) = testBit ss . fromEnum

raiseSignal :: Signal -> Number p r -> Number p r
raiseSignal sig n = let n' = modifyContext (setSignal sig) n
                    in trapHandler (context n') sig n'

{- $doctest
prop> x + x == x * (2 :: Decimal)
prop> isFinite x ==> x - x == (0 :: Decimal)
-}

{- $doctest-read
prop> rep (read "0")         == N (0,0,0)
prop> rep (read "0.00")      == N (0,0,-2)
prop> rep (read "123")       == N (0,123,0)
prop> rep (read "-123")      == N (1,123,0)
prop> rep (read "1.23E3")    == N (0,123,1)
prop> rep (read "1.23E+3")   == N (0,123,1)
prop> rep (read "12.3E+7")   == N (0,123,6)
prop> rep (read "12.0")      == N (0,120,-1)
prop> rep (read "12.3")      == N (0,123,-1)
prop> rep (read "0.00123")   == N (0,123,-5)
prop> rep (read "-1.23E-12") == N (1,123,-14)
prop> rep (read "1234.5E-4") == N (0,12345,-5)
prop> rep (read "-0")        == N (1,0,0)
prop> rep (read "-0.00")     == N (1,0,-2)
prop> rep (read "0E+7")      == N (0,0,7)
prop> rep (read "-0E-7")     == N (1,0,-7)
prop> rep (read "inf")       == I (0)
prop> rep (read "+inFiniTy") == I (0)
prop> rep (read "-Infinity") == I (1)
prop> rep (read "NaN")       == Q (0)
prop> rep (read "-NAN")      == Q (1)
prop> rep (read "SNaN")      == S (0)
xxxx> rep (read "Fred")      == Q (0)

prop> fmap rep (read "Just 123")     == Just (N (0,123,0))
prop> fmap rep (read "Just (-12.0)") == Just (N (1,120,-1))
-}

{- $setup
>>> :load test/Arbitrary.hs
>>> import Numeric.Decimal.Number
>>> import qualified Numeric.Decimal.Number as N

>>> type Decimal = BasicDecimal

>>> data Rep = N (Int, Coefficient, Exponent) | I Int | Q Int | S Int deriving Eq
>>> :{
    let rep n = case n :: Decimal of
            Num  { sign = s, coefficient = c, N.exponent = e } -> N (fromEnum s, c, e)
            Inf  { sign = s } -> I (fromEnum s)
            QNaN { sign = s } -> Q (fromEnum s)
            SNaN { sign = s } -> S (fromEnum s)
:}
-}
