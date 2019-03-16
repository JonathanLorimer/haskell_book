module CH8 where

import Data.List (intersperse)
import Data.Char (digitToInt)
factorial :: Int -> Int
factorial 2 = 2
factorial x = x * factorial (x - 1)

sum' :: Int -> Int
sum' 0 = 0
sum' x = x + sum' (x - 1)

memoize :: (Int -> Int) -> (Int -> Int)
memoize f = (map f [0..] !!)

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibM :: Int -> Int
fibM = memoize fib

fibM' :: Int -> Int
fibM' = f    
    where
        f i = xs !! i
        xs = map fib' [0..]
        fib' 1 = 1
        fib' 2 = 1
        fib' i =  f(i - 2) + f(i - 1)
    

cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny 
appedCatty = cattyConny "woops"
frappe = flippy "haha"

l =  cattyConny (frappe "pink") 
                (cattyConny "green" (appedCatty "blue"))

sumMult :: (Integral a) => a -> a -> a
sumMult _ 0 = 0
sumMult n m = n + sumMult n (m - 1)

data DividedResult =
      Result (Integer, Integer)
    | DividedByZero 
        deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
    | denom == 0 = DividedByZero
    | num < 0 && denom < 0 = go (abs num) (abs denom) 0 '+'
    | num < 0 || denom < 0 = go (abs num) (abs denom) 0 '-'
    | otherwise = go num denom 0 '+'
        where 
        go n d count sign
            | n < d = 
                if (sign == '+') 
                then Result (count, n)
                else Result (-count, n)
            | otherwise =
                go (n - d) d (count + 1) sign


mc91 :: Int -> Int
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 $ mc91 (x + 11)

digitToWord :: Int -> String 
digitToWord = 
    (["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !!)

wordNumber :: Int -> String
wordNumber = 
    concat . intersperse "-" . (map $ digitToWord . digitToInt) . show