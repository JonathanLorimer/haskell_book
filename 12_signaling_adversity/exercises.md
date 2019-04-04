## Chapter Exercises

1. `*`
2. `f: * -> *, a: *, f a: *`

#### String Processing

1.

```haskell
import Data.List
notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . go . words
    where
        go [] = []
        go (x:xs) = case notThe x of
                Nothing -> "a":(go xs)
                Just x -> x:(go xs)
```

2.

```haskell
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . (foldl' go (False, 0)) . words
    where
        go (bool, num) str = case notThe str of
                Nothing -> (True, num)
                Just (x:_) -> if x `elem` "aeiou" && bool then (False, num + 1) else (False, num)
```

3.

```haskell
countVowels :: String -> Int
countVowels = foldl' go 0
            where
                go num c = if c `elem` "aeiou" then num + 1 else num
```

#### Validate the Word

```haskell
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
    | ((countVowels str) * 2) > (length str) = Nothing
    | otherwise = Just (Word' str)
```

#### It's Only Natural

```haskell
data Nat =
            Zero
          | Succ Nat
          deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat i
            | i < 0 = Nothing
            | otherwise = Just
                        . (foldl' succeed Zero)
                        . (take (fromInteger i))
                        . repeat $ Succ
                where
                    succeed z f = f z
```

#### Simple Boolean Checks for Maybe

1.

```haskell
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust
```

2.

```haskell
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y
```

3.

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id
```

4.

```haskell
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])
```

5.

```haskell
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
    where
        go Nothing xs = xs
        go (Just x) xs = x:xs
```

6.

```haskell
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe  = foldr go (Just [])
        where
            go Nothing _ = Nothing
            go _ Nothing = Nothing
            go (Just x) (Just xs) = Just (x:xs)
```

#### Small library for Either

1.

```haskell
lefts' :: [Either a b] -> [a]
lefts' = foldr go []
    where
        go (Left a) xs = a:xs
        go _ xs = xs
```

2.

```haskell
rights' :: [Either a b] -> [b]
rights' = foldr go []
    where
        go (Right b) xs = b:xs
        go _ xs = xs
```

3.

```haskell
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = lefts' &&& rights'
```

4.

```haskell
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)
```

5.

```haskell
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b
```

6.

```haskell
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
```

#### Write your own iterate and unfoldr

1.

```haskell
myIterate :: (a -> a) -> a -> [a]
myIterate f a = f a : (myIterate f (f a))
```

2.

```haskell
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = ((getFst . f) b) : (myUnfoldr f ((getSnd . f) b))
        where
            getFst (Just (x,y)) = x
            getSnd (Just (x,y)) = y
```

3.

```haskell
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x
```

#### Finally something other than a list!
