# Chapter Exercises

```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```

#### 1.

```haskell
length :: [a] -> Int
```

#### 2.

a. 5
b. 3
c. 2
d. 5

#### 3.

this will return an error

```haskell
6 / length [1,2,3]
```

#### 4.

```haskell
6 `div` length [1,2,3]
```

#### 5.

```haskell
Boolean
True
```

#### 6.

False

#### 7.

1. True
2. can't have lists of items of different types
3. 5
4. False
5. can't use the && operator takes two boolean values

#### 8.

```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
```

#### 9.

```haskell
myAbs :: Integer -> Integer
myAbs x = if x < 0
    then x * -1
    else x
```

#### 10.

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

## Correcting Syntax

#### 1.

x should be in back ticks not quotes

#### 2.

\x -> x

#### 3.

f (a, b) = a

## Match the function names to their types

1. C
2. B
3. A
4. D
