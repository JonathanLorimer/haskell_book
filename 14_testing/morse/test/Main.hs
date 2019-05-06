module Main where
import qualified MorseTest as M
import qualified WordsNumbersTest as W
import qualified Properties as P

main :: IO ()
main = do 
    M.main
    W.main
    P.main