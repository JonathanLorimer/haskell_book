## Questions

1. With regard to exercise enumFromTo is pred less performant than succ?
2. With regard to pg. 320 (sprint mode in ghci), when I construct an infinite list, or even a finit lis using `[a..b]` syntax :sprint always yields \_ . However, when I construct the list with enumFromTo :sprint will show me the values I have evaluted. Why is this, and does it have something to do with the Enum typeclass?
3. Could use some clarification on spine strictness, particularly around pattern matching and length function

p.s. also doesn't seem to work with numbers

## Exercise

#### EnumFromTo

```haskell
-- | Enum From To
eftBool :: Bool -> Bool -> [Bool]
eftBool True False = [True, False]
eftBool False True = []
eftBool bool _ = [bool]

-- Question? is pred less performant than succ?

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd = go []
    where
        go l o1 o2
            | o1 > o2 = l
            | o1 == o2 = o1:l
            | otherwise = go (o2:l) o1 $ pred o2

eftInt :: Int -> Int -> [Int]
eftInt = go []
    where
        go l n1 n2
            | n1 > n2 = []
            | n1 == n2 = n1:l
            | otherwise = go (n2:l) n1 $ pred n2

eftChar :: Char -> Char -> [Char]
eftChar = go []
    where
        go l c1 c2
            | c1 > c2 = []
            | c1 == c2 = c1:l
            | otherwise = go (c2:l) c1 $ pred c2
```

#### Exercises: Thy Fearful Symmetry

1.

```haskell
myWords :: String -> [String]
myWords = go []
    where
        go xs [] = xs
        go xs string = go ((extractWord string):xs) $ removeWordAndSpace string

extractWord :: String -> String
extractWord = takeWhile (/= ' ')

removeWordAndSpace :: String -> String
removeWordAndSpace = (drop 1) . dropWhile (/= ' ')
```

2.

```haskell
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = go []
    where
        go xs [] = reverse xs
        go xs string = go ((extractLine string):xs) $ removeLineAndNewline string

extractLine :: String -> String
extractLine = takeWhile (/= '\n')

removeLineAndNewline :: String -> String
removeLineAndNewline = (drop 1) . dropWhile (/= '\n')
```

3.

```haskell
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitOnChar :: Char -> String -> [String]
splitOnChar char = go []
    where
        go xs [] = reverse xs
        go xs string =
            go ((extractBeforeChar char string):xs)
            $ removeOnCharInclusive char string

extractBeforeChar :: Char -> String -> String
extractBeforeChar char = takeWhile (/= char)

removeOnCharInclusive :: Char -> String -> String
removeOnCharInclusive char = (drop 1) . dropWhile (/= char)

myWords :: String -> [String]
myWords = splitOnChar ' '

myLines :: String -> [String]
myLines = splitOnChar '\n'
```

#### Comprehend Thy Lists

1. `[4,16,36,64,100]`
2. `[(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]`
3. `[(1,64),(1,81),(1,100),(4,64),(4,81)]`

#### Square Cube

1.

```haskell
-- if you want all possible combinations
[(x,y) | x <- mySqr, y <- myCube]

-- if you want combinations on indices
zip mySqr myCube
```

2.

```haskell
-- if you want all possible combinations
[(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- if you want combinations on indices
[x | x <- zip mySqr myCube, fst x < 50, snd x < 50]
```

3.

```haskell
length [x | x <- zip mySqr myCube, fst x < 50, snd x < 50]
```

#### Bottom Madness

1. Blow up
2. `[1]`
3. Blow up
4. `3`
5. Blow up
6. `[2]`
7. Blow up
8. `[1]`
9. `[1,3]`
10. Blow up

#### Intermission

1. NF
2. WHNF
3. Neither
4. Neither
5. Neither
6. Neither
7. WHNF

#### More Bottoms

1. Blow up
2. `[2]`
3. Blow up
4. Replaces vowels with true and consonants with false
5. a. `[1,4,9,16,25,36,49,64,81,100]`
   b. `[1,10,20]`
   c. `[15,15,15]`

#### Filtering

1.

```haskell
multsOf3 :: [Int] -> [Int]
multsOf3 = filter $ ((==) 0) . (`mod` 3 )
```

2.

```haskell
(length . multsOf3)
```

3.

```haskell
removeArticle :: String -> [String]
removeArticle = (filter (not . isArticle)) . words

isArticle :: String -> Bool
isArticle "a" = True
isArticle "an" = True
isArticle "the" = True
isArticle _ = False
```

#### Zipping Exercises

1.

```haskell
zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)
```

2.

```haskell
zipWith' :: (a -> b -> c)
        -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)
```

3.

```haskell
zipFromZipWith :: [a] -> [b] -> [(a, b)]
zipFromZipWith = zipWith (,)
```

## Chapter Exercises

#### Data.Char

2. isUpper
3.

```haskell
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = ((toUpper x):xs)
```

4.

```haskell
capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = ((toUpper x):(capitalizeAll xs))
```

5./6.

```haskell
capHead :: String -> Char
capHead = toUpper . head
```

#### Ciphers

```haskell
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
```

#### Writing your own standard functions

1.

```haskell
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs
```

2.

```haskell
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x)
                 then True
                 else myAny f xs
```

3.

```haskell
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = if el == x
    then True
    else myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' el = any (el ==)
```

4.

```haskell
myReverse :: [a] -> [a]
myReverse = go []
    where
        go :: [a] -> [a] -> [a]
        go new [] = new
        go new (x:xs) = go (x:new) xs
```

5.

```haskell
squish :: [[a]] -> [a]
squish = foldl (++) []
```

6.

```haskell
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . go f
        where
            go _ [] = []
            go f (x:xs) = (f x):(go f xs)
```

7.

```haskell
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (++ [])
```

8.

```haskell
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
            where
                go f c [] = c
                go f c (x1:xs)
                    | f c x1 == GT = go f c xs
                    | f c x1 == LT = go f x1 xs
                    | otherwise = go f x1 xs
```

9.

```haskell
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x xs
            where
                go f c [] = c
                go f c (x1:xs)
                    | f c x1 == LT = go f c xs
                    | f c x1 == GT = go f x1 xs
                    | otherwise = go f x1 xs
```

10.

```haskell
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
```
