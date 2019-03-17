module Cipher where
import Data.Char

encodeChar :: Int -> Char -> Char
encodeChar num char
            | ordering >= 97 && ordering <= 122 = go 96 ordering
            | ordering >= 65 && ordering <= 90 = go 64 ordering
            | otherwise = char
               where
                ordering = ord char
                go :: Int -> Int -> Char
                go offset = chr 
                    . (+) offset
                    . (\x -> if x > 26 then mod x 26 else x)
                    . (+) num
                    . (flip (-) offset)

decodeChar :: Int -> Char -> Char
decodeChar num char
            | ordering >= 97 && ordering <= 122 = go 97 ordering
            | ordering >= 65 && ordering <= 90 = go 65 ordering
            | otherwise = char
               where
                ordering = ord char
                go :: Int -> Int -> Char
                go offset = chr 
                    . (+) offset
                    . (\x -> if x < 0 then 26 - (mod (abs x) 26) else x)
                    . (flip (-) (offset + num))

encode :: Int -> String -> String
encode num = map $ encodeChar num

decode :: Int -> String -> String
decode num = map $ decodeChar num