module Applicative.ZipList where

import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                )

newtype ZipList' a = ZipList' [a]
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' = let (ZipList' l) = xs in take 3000 l
        ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure z = ZipList' [z]
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ go fs xs
        where go (f : fs') (x : xs') = f x : (fs' <*> xs')