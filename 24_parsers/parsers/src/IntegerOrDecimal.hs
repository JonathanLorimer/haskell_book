{-# LANGUAGE OverloadedStrings #-}

module IntegerOrDecimal where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Ratio ((%))
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type PError = ParseErrorBundle Text Void

isInteger :: Parser Char
isInteger = satisfy isDigit

parseInteger :: Parser Integer
parseInteger = do
  space
  int <- many isInteger
  space
  eof
  return $ read int

parseDecimal :: Parser Rational
parseDecimal = do
  numerator <- many isInteger
  _ <- char '/'
  denominator <- many isInteger
  case denominator of
    "0" -> fail "Denominator cannot be zero"
    _ -> return $ (read numerator) % (read denominator)

decimalOrInteger :: Parser (Either Rational Integer)
decimalOrInteger = do
  p <- eitherP (try parseDecimal) (try parseInteger)
  return p

parseDorI :: Text -> Either PError (Either Rational Integer)
parseDorI = runParser decimalOrInteger "<IntegerOrDecimal>"
