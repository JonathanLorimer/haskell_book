module Ciphers where
import           Data.Char

vigenere :: IO ()
vigenere = do
  putStr "Enter 1 to encode, enter 2 to decode: "
  encodeOrDecode <- getLine
  putStr "Please enter keyword: "
  keyword <- getLine
  putStr "Please enter phrase: "
  phrase <- getLine
  case encodeOrDecode of
    "1" -> print $ vigenereEncode phrase keyword
    "2" -> print $ vigenereDecode phrase keyword
    _   -> print "WTH"

caesar :: IO ()
caesar = do
  putStr "Enter 1 to encode, enter 2 to decode: "
  encodeOrDecode <- getLine
  putStr "Please enter number key: "
  num <- getLine
  putStr "Please enter phrase: "
  phrase <- getLine
  case encodeOrDecode of
    "1" -> print $ encode (read num) phrase
    "2" -> print $ decode (read num) phrase
    _   -> print "WTH"

encodeChar :: Int -> Char -> Char
encodeChar num char | ordering >= 97 && ordering <= 122 = go 96 ordering
                    | ordering >= 65 && ordering <= 90  = go 64 ordering
                    | otherwise                         = char
 where
  ordering = ord char
  go :: Int -> Int -> Char
  go offset =
    chr
      . (+) offset
      . (\x -> if x > 26 then mod x 26 else x)
      . (+) num
      . (flip (-) offset)

decodeChar :: Int -> Char -> Char
decodeChar num char | ordering >= 97 && ordering <= 122 = go 97 ordering
                    | ordering >= 65 && ordering <= 90  = go 65 ordering
                    | otherwise                         = char
 where
  ordering = ord char
  go :: Int -> Int -> Char
  go offset =
    chr
      . (+) offset
      . (\x -> if x < 0 then 26 - (mod (abs x) 26) else x)
      . (flip (-) (offset + num))

encode :: Int -> String -> String
encode num = map $ encodeChar num

decode :: Int -> String -> String
decode num = map $ decodeChar num

vigenereEncode :: String -> String -> String
vigenereEncode phrase keyword = zipWith encodeChar numbers phrase
  where numbers = map ((flip (-) 65) . fromEnum) keyword

vigenereDecode :: String -> String -> String
vigenereDecode phrase keyword = zipWith decodeChar numbers phrase
  where numbers = map ((flip (-) 65) . fromEnum) keyword
