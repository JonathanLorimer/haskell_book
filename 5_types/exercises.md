# Exercises

## Type Matching

| letter func | letter type | function | type signature            |
| ----------- | ----------- | -------- | ------------------------- |
| a)          | c)          | not      | `Bool -> Bool`            |
| b)          | d)          | length   | `[a] -> Int`              |
| c)          | b)          | concat   | `[[a]] -> [a]`            |
| d)          | a)          | head     | `[a] -> a`                |
| e)          | e)          | (<)      | `Ord a => a -> a -> Bool` |

## Type Arguments

1. a
2. d
3. d
4. c
5. a
6. e
7. d
8. a
9. c

## Parametricity

1. cannot be done
2. ans

```haskell
constForSameType :: a -> a -> a
constForSameType value1 _ = value1

seqForSameType :: a -> a -> a
seqForSameType _ value2 = value2
```

3. 1 implementation, no

## Apply Yourself

1.

```haskell
[Char] -> [Char]
```

2.

```haskell
Fractional a => a -> a
```

3.

```haskell
Int -> [Char]
```

4.

```haskell
Int -> Bool
```

5.

```haskell
Char -> Bool
```

## Chapter Exercises

#### Multiple Choice

1. c)
2. a)
3. b)
4. c)

#### Determine the Type

1.

- a) `Num a => a`
- b) `Num a => (a , [Char])`
- c) `(Integer, [Char])`
- d) `Bool`
- e) `Int`
- f) `Bool`

2. `Num a => a`
3. `Num a => a -> a`
4. `Fractional a => a`
5. `[Char]`

#### Does it Compile

1. bigNum is not a function it is a value
2. No issues
3. can't apply 10 to b (5)
4. b isn't defined yet

#### Type variable or specific type constructor?

2. fully Polymorphic -> concrete -> concrete
3. fully Polymorphic -> constrained Polymorphic -> concrete
4. fully Polymorphic -> fully Polymorphic -> concrete

#### Write a type signature

1. `[a] -> a`
2. `Ord a => a -> a -> Bool`
3. `(a, b) -> b`

#### Given a type, write the functions

1. `i x = x`
2. `c = const`
3. `c'' = const`
4. `c' = seq`
5.

```haskell
r1 (a:as) = as
r2 (a:b:bs) = bs
r2 (a:b:bs) = bs:[a]
```

6.

```haskell
co f1 f2 = f1 . f2
```

7.

```haskell
a f x = x
```

8.

```haskell
a f = f
```

#### Fix it

1.

```haskell
fstString :: [Char] ++ [Char]
-- should be
fstString :: [Char] -> [Char]

where x = "Singin"
      x = "Somewhere"
-- should be
where x = "Singin"
      y = "Somewhere"
```

2. `if (x < y)`

3. `print $ 1 + 2`

#### Type-Kwon-Do

1.

```haskell
h = g . f
```

2.

```haskell
e = w . q
```

3.

```haskell
xForm (a,b) = (xz a, yz b)
```

4.

```haskell
munge f g = fst . g . f
```
