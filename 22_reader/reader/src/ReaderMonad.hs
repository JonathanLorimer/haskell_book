{-# LANGUAGE InstanceSigs #-}

module ReaderMonad where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader a) = Reader $ f . a
    
instance Applicative (Reader r) where 
    pure :: a -> Reader r a
    pure a = Reader $ const a
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)
-- | 1.
instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

-- | 2.

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address 
               } deriving (Eq, Show)

rDogName :: Reader Person DogName
rDogName = Reader { runReader = dogName }

rAddress :: Reader Person Address
rAddress = Reader { runReader = address }

-- rDog :: Reader (Address -> DogName -> Dog) Dog
-- rDog = Reader { runReader = \n a -> Dog n a }

p2D :: Reader Person Dog
p2D = do
    n <- rDogName
    a <- rAddress
    return (Dog n a)

getDogRM :: Person -> Dog
getDogRM = runReader p2D

-- $> getDogRM (Person (HumanName "Jonathan") ( DogName "Wally") ( Address "236 Wychwood"))
