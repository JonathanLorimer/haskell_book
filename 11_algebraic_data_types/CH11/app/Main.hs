module Main where

import Data.List.Split
import Data.List
import Data.Char


main :: IO ()
main = undefined

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs


capitalizeParagraph :: String -> String
capitalizeParagraph para = intercalate ". " . map capitalizeWord . splitOn ". " $ (capitalizeWord para)