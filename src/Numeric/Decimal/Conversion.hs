
-- | The functions in this module implement conversions between 'Number' and
-- 'String' as described in the /General Decimal Arithmetic Specification/.
--
-- Because these functions are also used to implement 'Show' and 'Read' class
-- methods, it is not usually necessary to import this module except to use
-- the 'toEngineeringString' function.

module Numeric.Decimal.Conversion
       ( -- * Numeric string syntax
         -- $numeric-string-syntax

         -- * Conversion to numeric string
         toScientificString
       , toEngineeringString

         -- * Conversion from numeric string
       , toNumber
       ) where

import Prelude hiding (exponent, round)

import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, toLower, toUpper)
import Data.List (foldl')
import Text.ParserCombinators.ReadP (ReadP, char, many, many1, option, optional,
                                     satisfy)

import Numeric.Decimal.Number
import Numeric.Decimal.Precision
import Numeric.Decimal.Rounding

-- | Convert a number to a string, using scientific notation if an exponent is
-- needed.
toScientificString :: Number p r -> ShowS
toScientificString = showNumber exponential

  where exponential :: Exponent -> String -> Exponent -> ShowS
        exponential e (d1:ds@(_:_)) _ = showChar d1 . showChar '.' .
                                        showString ds . showExponent e
        exponential e     ds        _ = showString ds . showExponent e

-- | Convert a number to a string, using engineering notation if an exponent
-- is needed.
toEngineeringString :: Number p r -> ShowS
toEngineeringString = showNumber exponential

  where exponential :: Exponent -> String -> Exponent -> ShowS
        exponential e ds@"0" _ = showString ds' . showExponent (e + adj)
          where adj = (3 - e `mod` 3) `mod` 3
                ds' | adj > 0   = '0' : '.' : replicate (fromIntegral adj) '0'
                    | otherwise = ds
        exponential e ds cl = shift adj (e - adj) ds'
          where adj = e `mod` 3
                ds' | cl - 1 < adj = ds ++
                      replicate (fromIntegral (adj - cl + 1)) '0'
                    | otherwise    = ds

        shift :: Exponent -> Exponent -> String -> ShowS
        shift 2 e (d1:d2:d3:ds@(_:_)) = showChar d1 . showChar d2 .
                                        showChar d3 . showChar '.' .
                                        showString ds . showExponent e

        shift 1 e (d1:d2:ds@(_:_))    = showChar d1 . showChar d2 .
                                        showChar '.' .
                                        showString ds . showExponent e

        shift 0 e (d1:ds@(_:_))       = showChar d1 . showChar '.' .
                                        showString ds . showExponent e

        shift _ e     ds              = showString ds . showExponent e

showNumber :: (Exponent -> String -> Exponent -> ShowS)
           -> Number p r -> ShowS
showNumber exponential num = signStr . case num of
  Num { coefficient = c, exponent = e }
    | e <= 0 && ae >= -6 -> nonExponential
    | otherwise          -> exponential ae cs cl

    where cs  = show c                   :: String
          cl  = fromIntegral (length cs) :: Exponent
          ae  = e + cl - 1               :: Exponent

          nonExponential :: ShowS
          nonExponential
            | e == 0    = showString cs
            | -e < cl   = let (ca, cb) = splitAt (fromIntegral $ cl + e) cs
                          in showString ca . showChar '.' . showString cb
            | otherwise = showChar '0' . showChar '.' .
              showString (replicate (fromIntegral $ -e - cl) '0') .
              showString cs

  Inf  {             } -> showString "Infinity"
  QNaN { payload = p } -> showString  "NaN" . diag p
  SNaN { payload = p } -> showString "sNaN" . diag p

  where signStr :: ShowS
        signStr = showString $ case sign num of
          Pos -> ""
          Neg -> "-"

        diag :: Payload -> ShowS
        diag 0 = showString ""
        diag d = shows d

showExponent :: Exponent -> ShowS
showExponent e
  | e == 0    = id  -- do not show zero exponent
  | e <  0    = indicator .                exps
  | otherwise = indicator . showChar '+' . exps
  where indicator = showChar 'E' :: ShowS
        exps      = shows e      :: ShowS

-- | Convert a string to a number, as defined by its abstract representation.
-- The string is expected to conform to the numeric string syntax described
-- here.
toNumber :: (Precision p, Rounding r) => ReadP (Number p r)
toNumber = round <$> (parseSign flipSign <*> parseNumericString)

  where parseSign :: (a -> a) -> ReadP (a -> a)
        parseSign negate = char '-' *> pure negate
          <|> optional (char '+') *> pure id

        parseNumericString :: ReadP (Number p r)
        parseNumericString = parseNumericValue <|> parseNaN

        parseNumericValue :: ReadP (Number p r)
        parseNumericValue = parseDecimalPart <*> option 0 parseExponentPart
          <|> parseInfinity

        parseDecimalPart :: ReadP (Exponent -> Number p r)
        parseDecimalPart = digitsWithPoint <|> digitsWithOptionalPoint

          where digitsWithPoint = do
                  digits <- many1 parseDigit
                  char '.'
                  fracDigits <- many parseDigit
                  return $ \e ->
                    Num { context = defaultContext
                        , sign = Pos
                        , coefficient = readDigits (digits ++ fracDigits)
                        , exponent = e - fromIntegral (length fracDigits)
                        }

                digitsWithOptionalPoint = fractionalDigits <|> wholeDigits

                fractionalDigits = do
                  char '.'
                  fracDigits <- many1 parseDigit
                  return $ \e ->
                    Num { context = defaultContext
                        , sign = Pos
                        , coefficient = readDigits fracDigits
                        , exponent = e - fromIntegral (length fracDigits)
                        }

                wholeDigits = do
                  digits <- many1 parseDigit
                  return $ \e -> Num { context = defaultContext
                                     , sign = Pos
                                     , coefficient = readDigits digits
                                     , exponent = e
                                     }

        parseExponentPart :: ReadP Exponent
        parseExponentPart = do
          parseString "E"
          parseSign negate <*> (readDigits <$> many1 parseDigit)

        parseInfinity :: ReadP (Number p r)
        parseInfinity = do
          parseString "Inf"
          optional $ parseString "inity"
          return Inf { context = defaultContext, sign = Pos }

        parseNaN :: ReadP (Number p r)
        parseNaN = parseQNaN <|> parseSNaN

        parseQNaN :: ReadP (Number p r)
        parseQNaN = do
          p <- parseNaNPayload
          return QNaN { context = defaultContext, sign = Pos, payload = p }

        parseSNaN :: ReadP (Number p r)
        parseSNaN = do
          parseString "s"
          p <- parseNaNPayload
          return SNaN { context = defaultContext, sign = Pos, payload = p }

        parseNaNPayload :: ReadP Payload
        parseNaNPayload = do
          parseString "NaN"
          readDigits <$> many parseDigit

        parseDigit :: ReadP Int
        parseDigit = digitToInt <$> satisfy isDigit

        parseString :: String -> ReadP ()
        parseString = mapM_ $ \c -> char (toLower c) <|> char (toUpper c)

        readDigits :: Num c => [Int] -> c
        readDigits = foldl' (\a b -> a * 10 + fromIntegral b) 0

-- $numeric-string-syntax
--
-- (The following description is from the
-- /General Decimal Arithmetic Specification/.)
--
-- Strings which are acceptable for conversion to the abstract representation
-- of numbers, or which might result from conversion from the abstract
-- representation to a string, are called /numeric strings/.
--
-- A /numeric string/ is a character string that describes either a
-- /finite number/ or a /special value/.
--
-- *   If it describes a /finite number/, it includes one or more decimal
--     digits, with an optional decimal point. The decimal point may be embedded
--     in the digits, or may be prefixed or suffixed to them. The group of
--     digits (and optional point) thus constructed may have an optional sign
--     (“@+@” or “@-@”) which must come before any digits or decimal point.
--
--     The string thus described may optionally be followed by an “@E@”
--     (indicating an exponential part), an optional sign, and an integer
--     following the sign that represents a power of ten that is to be
--     applied. The “@E@” may be in uppercase or lowercase.
--
-- *   If it describes a /special value/, it is one of the case-independent
--     names “@Infinity@”, “@Inf@”, “@NaN@”, or “@sNaN@” (where the first two
--     represent /infinity/ and the second two represent /quiet NaN/ and
--     /signaling NaN/ respectively). The name may be preceded by an optional
--     sign, as for finite numbers. If a NaN, the name may also be followed by
--     one or more digits, which encode any diagnostic information.
--
-- No blanks or other white space characters are permitted in a numeric string.
--
-- == Examples
--
-- Some numeric strings are:
--
-- >     "0"          -- zero
-- >     "12"         -- a whole number
-- >    "-76"         -- a signed whole number
-- >     "12.70"      -- some decimal places
-- >     "+0.003"     -- a plus sign is allowed, too
-- >    "017."        -- the same as 17
-- >       ".5"       -- the same as 0.5
-- >     "4E+9"       -- exponential notation
-- >      "0.73e-7"   -- exponential notation, negative power
-- >     "Inf"        -- the same as Infinity
-- >     "-infinity"  -- the same as -Inf
-- >     "NaN"        -- not-a-Number
-- >     "NaN8275"    -- diagnostic NaN
--
-- == Notes
--
-- 1. A single period alone or with a sign is not a valid numeric string.
-- 2. A sign alone is not a valid numeric string.
-- 3. Significant (after the decimal point) and insignificant leading zeros
--    are permitted.

{- $doctest
prop> read' toNumber (show' toScientificString  x) == (x :: Decimal)
prop> read' toNumber (show' toEngineeringString x) == (x :: Decimal)
-}

{- $setup
>>> :load test/Arbitrary.hs
>>> import Numeric.Decimal
>>> import Numeric.Decimal.Conversion
>>> import Text.ParserCombinators.ReadP

>>> type Decimal = BasicDecimal

>>> let show' s = ($ "") . s
>>> :{
    let read' p str = case [ x | (x, "") <- readP_to_S p str ] of {
      [x] -> x; _ -> error "read failed" }
    :}
-}
