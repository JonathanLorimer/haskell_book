{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import Control.Arrow
import Data.List
import qualified Data.Map as Map


data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)


data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir


isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Either String Manufacturer
getManu (Car manufacturer _) = Right manufacturer
getManu (Plane _ _) = Left "Planes do not have a manufacturer"


-- class TooMany a where
--     tooMany :: a -> Bool
-- instance TooMany Int where
--     tooMany n = n > 42
    
-- instance TooMany Product where
--     tooMany (Product (i, s)) = i > 42

-- instance TooMany TwoFields where
--     tooMany (TwoFields (i, i')) = i + i' > 42

-- newtype Product = Product (Int, String) deriving Show
-- newtype TwoFields = TwoFields (Int, Int) deriving Show

-- instance (Num a, TooMany a) => TooMany (a, a) where
--     tooMany (x, y) = tooMany (x + y)


data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b 
                                       } deriving (Eq, Show)


data OperatingSystem =
                       GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang =
                Haskell
              | Agda
              | Idris
              | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang
                             } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill , Mac
                      , Windows
                      ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = Programmer <$> allOperatingSystems <*> allLanguages




data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
         1
         (Node Leaf 4 Leaf)

mapExpected = 
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

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


phrase = "MEET AT DAWN"
keyword = "ALLY AL LYAL"

vigenereEncode :: String -> String -> String
vigenereEncode phrase keyword = zipWith encodeChar numbers phrase
    where
        numbers = map ((flip (-) 65) . fromEnum) keyword
        
vigenereDecode :: String -> String -> String
vigenereDecode phrase keyword = zipWith decodeChar numbers phrase
    where
        numbers = map ((flip (-) 65) . fromEnum) keyword

-- As-patterns
isSubseqOf :: (Eq a) => [a]
                     -> [a]
                     -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xs') (y:ys)
        | x == y = isSubseqOf xs' ys
        | otherwise = isSubseqOf xs ys

-- Helper types
type Digit = Char
type Presses = Int
data Input = Input Digit Presses deriving (Show)

data Key = Key { keyVal   :: Digit
               , keyChars :: [Char]
               } deriving (Show)

-- Data Structures
data DaPhone = DaPhone [Key]

instance Eq Key where
    x == y = (keyVal x) == (keyVal y)

instance Ord Key where
    compare x y = compare (keyVal x) (keyVal y)

myPhone = 
    DaPhone [ Key '1' ""
            , Key '2' "abc"
            , Key '3' "def"
            , Key '4' "ghi"
            , Key '5' "jkl"
            , Key '6' "mno"
            , Key '7' "pqrs"
            , Key '8' "tuv"
            , Key '9' "wxyz"
            , Key '*' "^"
            , Key '0' " +_"
            , Key '#' ".,"
            ]

generatePhoneBook :: DaPhone -> Map.Map Char Input
generatePhoneBook (DaPhone xs) = foldl' enter Map.empty xs

enter :: Map.Map Char Input -> Key -> Map.Map Char Input
enter m k = fst $ foldl' insertChar (initialMap, 1) (keyChars k)
    where
        insertChar (map, presses) c = (Map.insert c (Input kVal presses) map, presses + 1)
        initialMap = Map.insert kVal (Input kVal ((+1) . length $ keyChars k)) m
        kVal = keyVal k

numberPresses :: Key -> Presses -> Char
numberPresses (Key keyVal keyChars) presses
    | times == 0 = keyVal
    | otherwise = keyChars !! times
    where times = rem presses $ length keyChars

-- With map passed explicitly
reverseTaps' :: DaPhone
            -> Char
            -> [Maybe Input]
reverseTaps' phone char
    | isUpper char = (Map.lookup '^' pb) : [Map.lookup (toLower char) pb]
    | otherwise = [Map.lookup char pb]
    where
        pb = generatePhoneBook myPhone

cellPhonesDead' :: DaPhone
               -> String
               -> [[Maybe Input]]
cellPhonesDead' phone = map (reverseTaps' phone)

translateConvo' :: DaPhone -> [String] -> [[[Maybe Input]]]
translateConvo' phone = map (cellPhonesDead' phone)

-- With Global Map
myPhoneBook = generatePhoneBook myPhone

reverseTaps :: Char
            -> [Maybe Input]
reverseTaps char
    | isUpper char = (Map.lookup '^' myPhoneBook) : [Map.lookup (toLower char) myPhoneBook]
    | otherwise = [Map.lookup char myPhoneBook]

cellPhonesDead :: String
               -> [[Maybe Input]]
cellPhonesDead = map reverseTaps

translateConvo :: [String] -> [[[Maybe Input]]]
translateConvo = map cellPhonesDead

convo :: [String] 
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]
  -- validButtons = "1234567890*#"

fingerTapsMaybe :: [Maybe Input] -> Presses
fingerTapsMaybe = foldl' go 0
    where
        go z (Just (Input _ p)) = z + p
        go z (Nothing) = z

fingerTaps :: [Input] -> Presses
fingerTaps = foldl' go 0
    where
        go z (Input _ p) = z + p

rankLetters :: String -> [(Int, Char)]
rankLetters = sort . (foldl' go []) . sort
    where
        go :: [(Int, Char)] -> Char -> [(Int, Char)]
        go [] c = (1,c):[]
        go y@((instances, char):xs) c = if c == char
                                        then (instances + 1, char):xs
                                        else (1, c):y

mostPopularLetter :: String -> Char
mostPopularLetter = snd . last . rankLetters
        
cost :: String -> Presses
cost = fingerTapsMaybe . reverseTaps . mostPopularLetter

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = undefined

data Expr
    = Lit Integer
    | Add Expr Expr

myExpr = Add (Add (Add (Lit 1) (Lit 2)) (Lit 3)) (Lit 3)

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e e') = (eval e) + (eval e')

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e e') = printExpr e ++ "+" ++ printExpr e'