module ChapterExercises where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

type PError = ParseErrorBundle T.Text Void

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Ord, Show)

semVerParser :: Parser SemVer
semVerParser = do
  major <- some digitChar
  minor <- parseNumVer
  patch <- parseNumVer
  _ <- many $ char '-'
  release <- parseRelease
  meta <- parseMeta
  return $ SemVer (read major) (read minor) (read patch) release meta

parseNumVer :: Parser String
parseNumVer = char '.' >> some digitChar

parseRelease :: Parser Release
parseRelease = parseNumOrString someTill '+'

parseMeta :: Parser Metadata
parseMeta = parseNumOrString sepBy '.'

parseNumOrString ::
     (Parser String -> Parser Char -> Parser [String])
  -> Char
  -> Parser [NumberOrString]
parseNumOrString comb c =
  try $ do
    m <- optional $ comb (try $ some alphaNumChar) $ char c
    let f = fmap stringToStrNum <$> m
    return $ fromMaybe [] f

stringToStrNum :: String -> NumberOrString
stringToStrNum s =
  if all isDigit s
    then (NOSI . read) s
    else NOSS s

parseSemVer :: T.Text -> Either PError SemVer
parseSemVer = runParser semVerParser "parseSemVer"
