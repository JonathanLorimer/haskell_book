## Exercises

#### Dog Types

1. Type Constructor
2. `* -> *`
3. `*`
4. `Doggies String`
5. `Doggies Integer`
6. `Doggies String`
7. Both
8. `DogueDeBordeaux :: DogueDeBordeaux a`
9. `DogueDeBordeaux :: DogueDeBordeaux String`

#### Vehicles

```haskell
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
```

1.

```haskell
myCar :: Vehicle
```

2. Given the following, define the functions:

```haskell
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar
```

3. Now we’re going to write a function to tell us the manufacturer of a piece of data:

```haskell

getManu :: Vehicle -> Either String Manufacturer
getManu (Car manufacturer _) = Right manufacturer
getManu (Plane _) = Left "Planes do not have a manufacturer"

```

4. You will get `Left "Planes do not have a manufacturer"`

5. All right. Let’s say you’ve decided to add the size of the plane as an argument to the Plane constructor. Add that to your datatypes in the appropriate places and change your data and functions appropriately.

```haskell
data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

getManu :: Vehicle -> Either String Manufacturer
getManu (Car manufacturer _) = Right manufacturer
getManu (Plane _ _) = Left "Planes do not have a manufacturer"
```

#### Cardinality

1. 1
2. 3
3. 65,536
4. Integer has an infinite cardinality while Int has a cardinality of 9223372036854775808 \* 2
5. 2 ^ 8 (i.e. base = numbers a single bit can encode, exponent = number of bits)

#### For Example

1. MakeExample :: Example, Data constructor Example isn't in scope
2. gives us info about the Data type, tells us we have an instance for show
3. `MakeExample :: Int -> Example'` takes an argument of type Int now

#### Logic Goats

1.

```haskell
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Product where
    tooMany (Product (i, s)) = i > 42

newtype Product = Product (Int, String) deriving Show

```

2.

```haskell
instance TooMany TwoFields where
    tooMany (TwoFields (i, i')) = i + i' > 42

newtype TwoFields = TwoFields (Int, Int) deriving Show
```

3.

```haskell
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)
```

#### Pity the Bool

1. 4
2. 258, overflow/underflow

#### How Does Your Garden Grow

1.

```haskell
data Garden = Garden Gardener Gardenia
            | Garden Gardener Daisy
            | Garden Gardener Rose
            | Garden Gardener Lilac
            deriving Show
```

#### Programmers

```haskell

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
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = Programmer <$> allOperatingSystems <*> allLanguages
```

#### The Quad

1. 8
2. 16
3. 256
4. 8
5. 16
6. 65,536

#### Writing Map for a Binary Tree

```haskell
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
```

#### Convert Binary Tree to List

```haskell
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]
```

#### Write foldr for a Binary Tree

```haskell
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node left val right) = foldTree f foldLeft right
        where
            foldLeft = foldTree f (f val z) left
```

## Chapter Exercises

1. a.
2. c.
3. b.
4. c.

#### Ciphers

```haskell
-- Re-used from Caesar Cipher
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

-- New Code
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
```

#### As-patterns

1.

```haskell
isSubseqOf :: (Eq a) => [a]
                     -> [a]
                     -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xs') (y:ys)
        | x == y = isSubseqOf xs' ys
        | otherwise = isSubseqOf xs ys
```

2.

```haskell
import Data.Char
import Control.Arrow

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (id &&& (map toUpper)) . words
```

#### Language Exercises

1.

```haskell
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
```

2.

```haskell
-- stack dep: - split
import Data.List.Split  (splitOn)
import Data.List        (intercalate)
import Data.Char        (toUpper)


main :: IO ()
main = undefined

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs


capitalizeParagraph :: String -> String
capitalizeParagraph para = intercalate ". " . map capitalizeWord . splitOn ". " $ (capitalizeWord para)
```

#### Phone exercise

1.

```haskell
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
```

2.

```haskell
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
```

3.

```haskell
fingerTapsMaybe :: [Maybe Input] -> Presses
fingerTapsMaybe = foldl' go 0
    where
        go z (Just (Input _ p)) = z + p
        go z (Nothing) = z

fingerTaps :: [Input] -> Presses
fingerTaps = foldl' go 0
    where
        go z (Input _ p) = z + p
```

4.

```haskell
mostPopularLetter :: String -> Char
mostPopularLetter = snd . last . sort . (foldl' go []) . sort
    where
        go :: [(Int, Char)] -> Char -> [(Int, Char)]
        go [] c = (1,c):[]
        go y@((instances, char):xs) c = if c == char
                                        then (instances + 1, char):xs
                                        else (1, c):y

cost :: String -> Presses
cost = fingerTapsMaybe . reverseTaps . mostPopularLetter
```

5.

```haskell
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = undefined
```

#### Hutton's Razor

1

```haskell
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e e') = (eval e) + (eval e')
```

2.

```haskell
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e e') = printExpr e ++ "+" ++ printExpr e'
```
