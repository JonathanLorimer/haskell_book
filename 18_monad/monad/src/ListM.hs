module ListM where

import qualified Control.Monad as MO

data List a = Nil
            | Cons a (List a)
                deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ (Nil) = Nil
    fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) _     Nil            = Nil
    (<*>) fList (Cons e eList) = apply fList e (fList <*> eList)
        where
            apply (Nil)          i list    = list
            apply (Cons f fList) i list    = apply fList i (Cons (f i) list)

instance Monad List where
    return = pure
    (>>=) m f = join . fmap f $ m

join :: List (List a) -> List a
join Nil            = Nil
join (Cons xs xss)  = concat xs (join xss)
        where
            concat (Nil) xss = xss
            concat (Cons x xs) xss = Cons x (concat xs xss)


showList' :: Int -> List String
showList' a = Cons (show a ++ "0") $ Cons (show a ++ "1") $ Cons (show a ++ "2")  $ Cons (show a ++ "3") $ Nil

add10List :: Int -> List Int
add10List a = Cons (a + 0) $ Cons (a + 1) $ Cons (a + 2)  $ Cons (a + 3) $ Nil

bind :: List Int -> List String 
bind = (>>= showList')

bind' :: List Int -> List String 
bind' = join . (fmap showList')

comp :: Int -> List String
comp = add10List MO.>=> showList'