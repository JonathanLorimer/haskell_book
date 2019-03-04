## Exercises

#### Grab Bag

1. a, b, c, d are functionally equivalent; a, b, c have the same type, d is different

2. d

3.

a.

```haskell
addOneIfOdd n = case odd n of
True -> f n
False -> n
    where f = \n = n + 1
```

b.

```haskell
addFive = \x -> \y -> (if x > y then y else x) + 5
```

c.

```haskell
mflip f x y = f y x
```

#### Variety Pack

1.

a.

```haskell
(a, b) -> a
```

b.

```haskell
([Char], Int)
```

c. k3

2.

```haskell
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))
```

#### Case Practice

1.

```haskell
functionC x y = case (x > y) of
    True -> x
    False -> y
```

2.

```haskell
ifEvenAdd2 n = case even n of
    True -> (n+2)
    False -> n
```

3.

```haskell
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0
```

#### Artful Dodgy

2. 11
3. 22
4. 21
5. 12
6. 11
7. 21
8. 21
9. 22
10. 31
11. 23

#### Guard Duty

1.

```haskell
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y> =0.9   = 'A'
    | y >=0.8   = 'B'
    | y >=0.7   = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100
```

2. No
3. b
4. `[a]`
5. `[a] -> Bool`
6. c
7. (Num a, Ord a) => a
8. (Num a, Ord a, Num b) => a -> b

#### Chapter Exercises

###### Multiple Choice

1. d
2. b
3. d
4. b
5. a

###### Let's write code

1.

a

```haskell
tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where xLast = x `divMod` 10
          d = mod (fst xLast) 10
```

b

yes, same type

c

```haskell
hunsDigit :: Integral a => a -> a
hunsDigit x = d
    where xLast = x `div` 100
          d = mod xLast 10
```

2.

```haskell
foldBool :: a -> a -> Bool -> a
foldBool x y flag =
    case flag of
        True -> y
        False -> x
```

```haskell
foldBool :: a -> a -> Bool -> a
foldBool x y flag
    | flag = y
    | otherwise = x
```

3.

```haskell
g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)
```

5.

```haskell
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show
```

6.

```haskell
main = do
    print (roundTrip 4 :: Int)
    print (id 4)
```
