# Exercises

## Exercise Eq Instances

1.

```haskell
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y
```

2.

```haskell
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x1 x2)(Two y1 y2) = x1 == y1 && x2 == y2
```

3.
```haskell
data StringOrInt =
      TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x)(TisAnInt y) = x == y
    (==) (TisAString x)(TisAString y) = x == y
    (==) (TisAnInt _)(TisAString _) = False
    (==) (TisAString _)(TisAnInt _) = False
````

4.

```haskell
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2)(Pair b1 b2) = a1 == b1 && a2 == b2
```

5.

```haskell
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1)(Tuple a2 b2) = a1 == a2 && b1 == b2
```

6.

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

7.

```haskell
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)(Hello y) = x == y
  (==) (Goodbye x)(Goodbye y) = x == y
  (==) (Hello _)(Goodbye _) = False
  (==) (Goodbye _)(Hello _) = False
```

## Exercises: Will They Work?

1. 5
2. LT
3. No, type is:

```haskell
compare :: Ord a => a -> a -> Ordering
```

not

```haskell
compare :: (Ord a, Ord b) => a -> b -> Ordering
```

4. False

## Chapter Exercises

#### Multiple Choice

1. c
2. b
3. a
4. c
5. a

#### Does it Typecheck

1. No instance for Show on Person
2. No instance for Eq
3. a. anything implementing Eq
   b. won't work because == takes `Eq a -> a -> a -> Bool`
   c. Will work
4. Yes, s1 is a partially applied type `s1 :: Object -> Sentence`

#### What can we do with the datatype declaration

1. Doesnt compile, should be `phew = Papu (Rocks "chases") (Yeah True)`
2. Compiles
3. Compiles
4. No instance for Ord Papu

#### Match the Types

1. No instance for Num a
2. Could not deduce Fractional a
3. Works
4. Works
5. Works
6. Works
7. Couldn't match a with actual type Int
8. Couldn't match a with actual type Int
9. Works
10. Works
11. Can't match a with Char

#### Type Kwon Do

1.

```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b
```

2.

```haskell
arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith f i a = (fromInteger i) + (f a)
```
