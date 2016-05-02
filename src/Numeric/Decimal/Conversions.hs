
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
toScientificString :: Number p r -> String
toScientificString num = signStr ++ case num of
  Num { coefficient = c, exponent = e }
    | e <= 0 && ae >= -6 -> nonExponential
    | otherwise          -> exponentialNotation cs
    where cs = show c
          cl = fromIntegral (length cs)
          ae = e + cl - 1
          nonExponential
            | e == 0    = cs
            | -e < cl   = let (ca, cb) = splitAt (fromIntegral $ cl + e) cs
                          in ca ++ '.' : cb
            | otherwise = '0' : '.' :
              replicate (fromIntegral $ -e - cl) '0' ++ cs
          exponentialNotation (d1:ds@(_:_)) = (d1:'.':ds) ++ exponent
          exponentialNotation     ds        =         ds  ++ exponent
          exponent = 'E' : aes
          aes | ae < 0    =       show ae
              | otherwise = '+' : show ae
  Inf{} -> "Infinity"
  QNaN { payload = p } ->  "NaN" ++ diag p
  SNaN { payload = p } -> "sNaN" ++ diag p

  where signStr :: String
        signStr = case sign num of
          Pos -> ""
          Neg -> "-"

        diag :: Payload -> String
        diag 0 = ""
        diag d = show d

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
