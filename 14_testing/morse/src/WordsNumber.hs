module WordsNumber
    ( digitToWord
    , wordNumber
    , digits
    )
where

import           Data.List                      ( intercalate )
import           Data.Char                      ( digitToInt )

digitToWord :: Int -> String
digitToWord =
    ([ "zero"
     , "one"
     , "two"
     , "three"
     , "four"
     , "five"
     , "six"
     , "seven"
     , "eight"
     , "nine"
     ] !!
    )

digits :: Int -> [Int]
digits = map digitToInt . show

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
