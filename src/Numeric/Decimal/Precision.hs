
module Numeric.Decimal.Precision
       ( Precision(..)
       , FinitePrecision

       , P1 , P2 , P3 , P4 , P5 , P6 , P7 , P8 , P9 , P10
       , P11, P12, P13, P14, P15, P16, P17, P18, P19, P20
       , P21, P22, P23, P24, P25, P26, P27, P28, P29, P30
       , P31, P32, P33, P34, P35, P36, P37, P38, P39, P40
       , P41, P42, P43, P44, P45, P46, P47, P48, P49, P50

       , P75, P100, P150, P200, P250, P300, P400, P500, P1000, P2000

       , PPlus1, PTimes2

       , PInfinite
       ) where

-- | Precision indicates the maximum number of significant decimal digits a
-- number may have.
class Precision p where
  -- | Return the precision of the argument, or 'Nothing' if the precision is infinite.
  precision :: p -> Maybe Int

-- | A subclass of precisions that are finite
class Precision p => FinitePrecision p

-- | A precision of unlimited significant digits
data PInfinite
instance Precision PInfinite where
  precision _ = Nothing

-- | A precision of 1 significant digit
data P1
instance Precision P1 where
  precision _ = Just 1
instance FinitePrecision P1

-- | A precision of (@p@ + 1) significant digits
data PPlus1 p
instance Precision p => Precision (PPlus1 p) where
  precision pp = (+ 1) <$> precision (minus1 pp)
    where minus1 :: PPlus1 p -> p
          minus1 = undefined
instance FinitePrecision p => FinitePrecision (PPlus1 p)

-- | A precision of (@p@ × 2) significant digits
data PTimes2 p
instance Precision p => Precision (PTimes2 p) where
  precision pp = (* 2) <$> precision (div2 pp)
    where div2 :: PTimes2 p -> p
          div2 = undefined
instance FinitePrecision p => FinitePrecision (PTimes2 p)

-- | A precision of 2 significant digits
type P2  = PTimes2 P1 ; type P3  = PPlus1 P2
-- ^ A precision of 3 significant digits

-- | Et cetera
type P4  = PTimes2 P2 ; type P5  = PPlus1 P4
type P6  = PTimes2 P3 ; type P7  = PPlus1 P6
type P8  = PTimes2 P4 ; type P9  = PPlus1 P8
type P10 = PTimes2 P5 ; type P11 = PPlus1 P10
type P12 = PTimes2 P6 ; type P13 = PPlus1 P12
type P14 = PTimes2 P7 ; type P15 = PPlus1 P14
type P16 = PTimes2 P8 ; type P17 = PPlus1 P16
type P18 = PTimes2 P9 ; type P19 = PPlus1 P18
type P20 = PTimes2 P10; type P21 = PPlus1 P20
type P22 = PTimes2 P11; type P23 = PPlus1 P22
type P24 = PTimes2 P12; type P25 = PPlus1 P24
type P26 = PTimes2 P13; type P27 = PPlus1 P26
type P28 = PTimes2 P14; type P29 = PPlus1 P28
type P30 = PTimes2 P15; type P31 = PPlus1 P30
type P32 = PTimes2 P16; type P33 = PPlus1 P32
type P34 = PTimes2 P17; type P35 = PPlus1 P34
type P36 = PTimes2 P18; type P37 = PPlus1 P36
type P38 = PTimes2 P19; type P39 = PPlus1 P38
type P40 = PTimes2 P20; type P41 = PPlus1 P40
type P42 = PTimes2 P21; type P43 = PPlus1 P42
type P44 = PTimes2 P22; type P45 = PPlus1 P44
type P46 = PTimes2 P23; type P47 = PPlus1 P46
type P48 = PTimes2 P24; type P49 = PPlus1 P48

type P50 = PTimes2 P25
type P62 = PTimes2 P31
type P74 = PTimes2 P37; type P75 = PPlus1 P74

type P100 = PTimes2 P50
type P124 = PTimes2 P62; type P125 = PPlus1 P124
type P150 = PTimes2 P75

type P200 = PTimes2 P100
type P250 = PTimes2 P125

type P300 = PTimes2 P150
type P400 = PTimes2 P200
type P500 = PTimes2 P250

type P1000 = PTimes2 P500
type P2000 = PTimes2 P1000
