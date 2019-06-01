# Exercises

## Be Kind

1. `*`
2. b: `* -> *` T: `* -> *`
3. `* -> * -> *`

## Heavy Lifting

1.

```haskell
    a = fmap (+1) $ read "[1]" :: [Int]
```

2.

```haskell
    b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
```

3.

```haskell
    c = (*2) . (\x -> x - 2)
```

4.

```haskell
    d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
```

5.

```haskell
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read . fmap ("123"++) . fmap show $ ioi
    in fmap (*3) changed
```

## Chapter Exercises

1. No
2. Yes
3. Yes
4. Yes
5. No

#### Rearrange the arguments to the type constructor

1.

```haskell
    data Sum b a = First a | Second b
```

2.

```haskell
    data Company a c b = DeepBlue a c | Something b
```

3.

```haskell
    data More b a = L a b a
                  | R b a b
                    deriving (Eq, Show)
```
