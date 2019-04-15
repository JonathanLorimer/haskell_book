import Data.List
import Control.Arrow

notThe :: String -> Maybe String 
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . go . words
    where
        go [] = []
        go (x:xs) = case notThe x of
                Nothing -> "a":(go xs)
                Just x -> x:(go xs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . (foldl' go (False, 0)) . words
    where
        go (bool, num) str = case notThe str of
                Nothing -> (True, num)
                Just (x:_) -> if x `elem` "aeiou" && bool then (False, num + 1) else (False, num)


countVowels :: String -> Int
countVowels = foldl' go 0
            where
                go num c = if c `elem` "aeiou" then num + 1 else num

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
    | ((countVowels str) * 2) > (length str) = Nothing
    | otherwise = Just (Word' str)



data Nat =
            Zero
          | Succ Nat
          deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat i
            | i < 0 = Nothing
            | otherwise = Just 
                        . (foldl' succeed Zero)
                        . (take (fromInteger i))
                        . repeat $ Succ
                where
                    succeed z f = f z
        
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])


catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
    where 
        go Nothing xs = xs
        go (Just x) xs = x:xs


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe  = foldr go (Just []) 
        where 
            go Nothing _ = Nothing
            go _ Nothing = Nothing
            go (Just x) (Just xs) = Just (x:xs)

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
    where
        go (Left a) xs = a:xs
        go _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
    where
        go (Right b) xs = b:xs
        go _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = lefts' &&& rights'


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = f a : (myIterate f (f a))


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = ((getFst . f) b) : (myUnfoldr f ((getSnd . f) b))
        where
            getFst (Just (x,y)) = x
            getSnd (Just (x,y)) = y


f b = Just (b, b+1)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) 
                  deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = mayybee Leaf go $ f a
            where
                go (a, b, a') = Node (unfold f a) b (unfold f a') 


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just ((x + 1), x, (x + 1))) 0
