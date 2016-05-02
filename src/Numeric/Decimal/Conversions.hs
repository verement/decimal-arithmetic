
module Numeric.Decimal.Conversions
       ( toScientificString
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

-- | Conversion to numeric string
toScientificString :: Number p r -> ShowS
toScientificString num = signStr . case num of
  Num { coefficient = c, exponent = e }
    | e <= 0 && ae >= -6 -> nonExponential
    | otherwise          -> exponentialNotation cs

    where cs = show c                   :: String
          cl = fromIntegral (length cs) :: Exponent
          ae = e + cl - 1               :: Exponent

          nonExponential :: ShowS
          nonExponential
            | e == 0    = showString cs
            | -e < cl   = let (ca, cb) = splitAt (fromIntegral $ cl + e) cs
                          in showString ca . showChar '.' . showString cb
            | otherwise = showChar '0' . showChar '.' .
              showString (replicate (fromIntegral $ -e - cl) '0') .
              showString cs

          exponentialNotation :: String -> ShowS
          exponentialNotation (d1:ds@(_:_)) = showChar d1 . showChar '.' .
                                              showString ds . exponent
          exponentialNotation     ds        = showString ds . exponent

          exponent :: ShowS
          exponent = showChar 'E' . aes

          aes :: ShowS
          aes | ae < 0    =                shows ae
              | otherwise = showChar '+' . shows ae

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

-- | Conversion from numeric string
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
