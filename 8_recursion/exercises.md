## Chapter Exercises

#### Review of Types

1. d.
2. b.
3. d.
4. b.

#### Review of Currying

1. woops mrow woohoo!
2. 1 mrow haha
3. woops mrow 2 mrow haha
4. woops mrow blue mrow haha
5. pink mrow haha mrow green mrow woops mrow blue
6. are mrow Pugs mrow awesome

#### Recursion

2.

```haskell
sum' :: Int -> Int
sum' 0 = 0
sum' x = x + sum' (x - 1)
```

3.

```haskell
sumMult :: (Integral a) => a -> a -> a
sumMult _ 0 = 0
sumMult n m = n + sumMult n (m - 1)
```

#### Fixing dividedBy

```haskell
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
```

#### McCarthy 91

```haskell
mc91 :: Int -> Int
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 $ mc91 (x + 11)
```

#### Numbers Into Words

```haskell
import Data.List (intersperse)
import Data.Char (digitToInt)

digitToWord :: Int -> String
digitToWord =
    (["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !!)

wordNumber :: Int -> String
wordNumber =
    concat . intersperse "-" . (map $ digitToWord . digitToInt) . show
```
