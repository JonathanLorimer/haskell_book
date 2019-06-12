# Chapter Exercises

```haskell
1. -- Type []
-- Methods
pure ::a-> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

2. -- Type IO
-- Methods
pure ::a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

3. -- Type (,) a
-- Methods
pure ::a -> (x,a)
(<*>) :: ( x, (a -> b) ) -> (x, a) -> (x, b)

4. -- Type (->) e
-- Methods
pure ::a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
```
