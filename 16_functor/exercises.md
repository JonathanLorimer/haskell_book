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
