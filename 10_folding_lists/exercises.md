## Exercises

#### Understanding Folds

1. c
2.

foldl (flip (_)) 1 [1..3]
(((1 _ 1) _ 2) _ 3)
(((1) _ 2) _ 3)
((2) \* 3)
(6)

3. c
4. a
5.

```
a) foldr (++) "" ["woot", "WOOT", "woot"]
b) foldr max [] ["fear is the little death"]
|| foldr max '\NUL' "fear is the little death"
c) foldr (&&) True [False, True]
d) foldr (||) False [False, True]
e) foldr ((++) . show) "" [1..5]
f) foldl const 'a' [1..5]
g) foldl const 0 "tacos"
h) foldr (flip const) 0 "burritos"
i) foldr (flip const) 'z' [1..5]
```

#### DB Processing

1.

```haskell
dbDateToList :: DatabaseItem -> [UTCTime] -> [UTCTime]
dbDateToList (DbDate time) xs = time:xs
dbDateToList _ xs = xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr dbDateToList []

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldl (flip dbDateToList) []
```

2.

```haskell
dbNumberToList :: DatabaseItem -> [Integer] -> [Integer]
dbNumberToList (DbNumber num) xs = num:xs
dbNumberToList _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr dbNumberToList []

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = foldl (flip dbNumberToList) []
```

3.

```haskell
maxDbDate :: DatabaseItem -> UTCTime -> UTCTime
maxDbDate (DbDate time1) time2 = max time1 time2
maxDbDate _ time = time

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr maxDbDate (UTCTime
                                (fromGregorian 0 0 0)
                                (secondsToDiffTime 34123)
                             )
```

4.

```haskell
sumDbNumber :: DatabaseItem -> Integer -> Integer
sumDbNumber (DbNumber num1) num2 = num1 + num2
sumDbNumber _ num = num

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumDbNumber 0
```

5.

```haskell
sumAndLength :: DatabaseItem -> (Integer, Integer) -> (Integer, Integer)
sumAndLength (DbNumber num) (sum, length) = (num + sum, length + 1)
sumAndLength _ sumLength = sumLength

divTuple :: (Integer, Integer) -> Double
divTuple (x,y) = (fromIntegral x) / (fromIntegral y)

avgDb :: [DatabaseItem] -> Double
avgDb = divTuple . foldr sumAndLength (0,0)
```

#### Scans Exercises

1.

```haskell
fibs = takeWhile (100 <) $ 1 : scanl (+) 1 fibs
```

2.

```haskell
fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs
```

3.

```haskell
factorial = scanl (+) 1 [2..]
```

## Chapter Exercises

#### Warm up Exercises

1.
a.

```haskell
permutate a b = (,,) <$> a <*> b <*> a
```

b.

```haskell
permutateWithP a b = ((,,) 'p') <$> a <*> b
```

c.

```haskell
nouns = ["bird", "dog", "kitten", "coffee", "girlfriend", "desk"]
verbs = ["walk", "attack", "eat", "study", "break"]

permutate a b = (,,) <$> a <*> b <*> a
```

2.

```haskell
seekritFunc :: String -> Int
seekritFunc x =
    (div . sum . map length . words $ x) $ (length . words) x

-- takes the number of words in the string and divides it by the sum of the length of the individual words
```

3.

```haskell
seekritFunc :: String -> Double
seekritFunc x =
    ((/) . fromIntegral . sum . map length . words $ x) $ (fromIntegral . length . words) x
```

#### Re-writing functions using folds

1.

```haskell
myOr :: [Bool] -> Bool
myOr = foldr pred False
    where
        pred _ True = True
        pred True _ = True
        pred False _ = False
```

2.

```haskell
myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr applyPred False
        where
            applyPred _ True = True
            applyPred item _ = pred item
```

3.

```haskell
myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr applyPred False
    where
        applyPred _ True = True
        applyPred item _ = (== x) item
```

4.

```haskell
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
```

5.

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
```

6.

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr applyPred []
    where
        applyPred item acc
            | pred item = item:acc
            | otherwise = acc
```

7.

```haskell
squish :: [[a]] -> [a]
squish = foldr (++) []
```

8.

```haskell
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
```

9.

```haskell
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
```

10.

```haskell
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = []
myMaximumBy f (x:xs) = foldr applyPred x xs
    where
        applyPred item acc
            | f item acc == GT = item
            | f item acc == LT = acc
            | otherwise = item
```

11.

```haskell
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = []
myMinimumBy f (x:xs) = foldr applyPred x xs
    where
        applyPred item acc
            | f item acc == LT = item
            | f item acc == GT = acc
            | otherwise = item
```
