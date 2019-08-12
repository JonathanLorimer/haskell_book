{-# LANGUAGE OverloadedStrings #-}

module IntegerOrDecimal where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Ratio ((%))
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

isInteger :: Parser Integer
isInteger = do
  read $ Prelude.concat $ many digitChar

isDecimal :: Parser Rational
isDecimal = do
  numerator <- isInteger
  _ <- char "/"
  denominator <- isInteger
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)
