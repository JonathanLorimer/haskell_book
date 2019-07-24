module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n 
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5  == 0 = "Buzz" 
    | n `mod` 3  == 0 = "Fizz"
    | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = foldr addResult [] list

addResult :: Integer -> [String] -> [String]
addResult n l = fizzBuzz n : l

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo s e = fizzbuzzList [s..e]