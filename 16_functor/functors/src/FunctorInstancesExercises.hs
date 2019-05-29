{-# LANGUAGE FlexibleInstances #-}
module FunctorInstancesExercises where


-- | 1.
data Quant a b = Finance
               | Desk a
               | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk  a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


-- | 2.
data K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- | 3. 
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K (f a)

-- | 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- | 5.
data LiftItOut f a =
    LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- | 6.
data Parappa f g a =
    DaWrappa (f a) (g a)

instance (Functor a, Functor b) => Functor (Parappa a b) where
    fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- | 7.
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething first second) =
        IgnoringSomething first (fmap f second)

-- | 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious first second third) =
        Notorious first second (fmap f third)

-- | 9.
data List a = Nil
            | Cons a (List a)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- | 10.
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat            = NoGoat
    fmap f (OneGoat x      ) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- | 11.

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read g   ) = Read (fmap f g)


