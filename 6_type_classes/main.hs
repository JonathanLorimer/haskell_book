import Data.List

data Trivial = Trivial | Trivial'

instance Eq Trivial where
    (==) Trivial Trivial = True
    (==) Trivial' Trivial' = True
    (==) _ _ = False

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where 
    (==) Mon Mon = True 
    (==) Tue Tue = True 
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True 
    (==) Sat Sat = True 
    (==) Sun Sun = True 
    (==) _ _ = False 

{- Exercise Eq Instances -}

-- | 1.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y 

-- | 2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x1 x2)(Two y1 y2) = x1 == y1 && x2 == y2

-- | 3.
data StringOrInt =
      TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x)(TisAnInt y) = x == y
    (==) (TisAString x)(TisAString y) = x == y
    (==) (TisAnInt _)(TisAString _) = False
    (==) (TisAString _)(TisAnInt _) = False

-- | 4.
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2)(Pair b1 b2) = a1 == b1 && a2 == b2

-- | 5.
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1)(Tuple a2 b2) = a1 == a2 && b1 == b2

-- | 6.
data Which a =
      ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x)(ThisOne y) = x == y
    (==) (ThatOne x)(ThatOne y) = x == y
    (==) (ThisOne _)(ThatOne _) = False
    (==) (ThatOne _)(ThisOne _) = False

-- | 7.
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)(Hello y) = x == y
  (==) (Goodbye x)(Goodbye y) = x == y
  (==) (Hello _)(Goodbye _) = False
  (==) (Goodbye _)(Hello _) = False



data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith f i a = (fromInteger i) + (f a)