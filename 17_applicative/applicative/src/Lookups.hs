module Lookups where

import           Control.Applicative            ( liftA2 )
import           Data.List                      ( elemIndex )

-- | 1
added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- | 2
a :: Maybe Integer
a = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

b :: Maybe Integer
b = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) a b

tupled' :: Maybe (Integer, Integer)
tupled' = (,) <$> a <*> b

-- | 3
c :: Maybe Int
c = elemIndex 3 [1, 2, 3, 4, 5]

d :: Maybe Int
d = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' c d

maxed' :: Maybe Int
maxed' = max' <$> c <*> d

-- | 4
es = [1, 2, 3]
fs = [4, 5, 6]

e :: Maybe Integer
e = lookup 3 $ zip es fs

f :: Maybe Integer
f = lookup 2 $ zip es fs

summed :: Maybe Integer
summed = sum <$> liftA2 (,) e f

summed' :: Maybe Integer
summed' = fmap sum $ (,) <$> e <*> f
