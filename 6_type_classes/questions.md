is there any way to return false for the comparison operation (==) when a type containing other types is compared with a type containing different types?

ex.

```haskell
data Pair a = Pair a a deriving (Show)

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2)(Pair b1 b2) = a1 == b1 && a2 == b2

```

this will just error out when Pair a a is compared with Pair b b and a !== b, but it would be better to have it return false?

another example is:

```haskell
data Which a =
      ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x)(ThisOne y) = x == y
    (==) (ThatOne x)(ThatOne y) = x == y
    (==) (ThisOne _)(ThatOne _) = False
    (==) (ThatOne _)(ThisOne _) = False
```

This is even trickier because if ThisOne and ThatOne (constructed with different types) are compared they will return false, but if ThisOne and ThisOne (constructed with different types) are compared they will error out.

I noticed a lot of questions surrounding this came up in ;'Exercises: Eq Instances'
