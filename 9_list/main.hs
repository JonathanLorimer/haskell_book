import Data.Char

-- | Enum From To
eftBool :: Bool -> Bool -> [Bool] 
eftBool True False = [True, False]
eftBool False True = []
eftBool bool _ = [bool]

-- Question? is pred less performant than succ?

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd = go []
    where 
        go l o1 o2
            | o1 > o2 = l
            | o1 == o2 = o1:l
            | otherwise = go (o2:l) o1 $ pred o2

eftInt :: Int -> Int -> [Int]
eftInt = go []
    where 
        go l n1 n2
            | n1 > n2 = []
            | n1 == n2 = n1:l
            | otherwise = go (n2:l) n1 $ pred n2

eftChar :: Char -> Char -> [Char]
eftChar = go []
    where 
        go l c1 c2
            | c1 > c2 = []
            | c1 == c2 = c1:l
            | otherwise = go (c2:l) c1 $ pred c2


-- | Thy Fearful Symmetry

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitWordOnChar :: Char -> String -> [String]
splitWordOnChar char = go []
    where
        go xs [] = reverse xs
        go xs string = 
            go ((extractBeforeChar char string):xs)
            $ removeOnCharInclusive char string

extractBeforeChar :: Char -> String -> String
extractBeforeChar char = takeWhile (/= char)

removeOnCharInclusive :: Char -> String -> String
removeOnCharInclusive char = (drop 1) . dropWhile (/= char)

myWords :: String -> [String]
myWords = splitWordOnChar ' '

myLines :: String -> [String]
myLines = splitWordOnChar '\n'


multsOf3 :: [Int] -> [Int]
multsOf3 = filter $ ((==) 0) . (`mod` 3 )

removeArticle :: String -> [String]
removeArticle = (filter (not . isArticle)) . words

isArticle :: String -> Bool
isArticle "a" = True
isArticle "an" = True
isArticle "the" = True
isArticle _ = False

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipWith' :: (a -> b -> c)
        -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

zipFromZipWith :: [a] -> [b] -> [(a, b)]
zipFromZipWith = zipWith (\x -> \y -> (x,y))

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = ((toUpper x):xs)

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = ((toUpper x):(capitalizeAll xs))

capHead :: String -> Char
capHead = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x)
                 then True
                 else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = if el == x
                   then True
                   else myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' el = any (el ==)

myReverse :: [a] -> [a]
myReverse = go []
    where
        go :: [a] -> [a] -> [a]
        go new [] = new
        go new (x:xs) = go (x:new) xs


squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . go f 
        where
            go _ [] = []
            go f (x:xs) = (f x):(go f xs)


squishAgain :: [[a]] -> [a]
squishAgain = squishMap (++ [])


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
            where
                go f c [] = c
                go f c (x1:xs)
                    | f c x1 == GT = go f c xs
                    | f c x1 == LT = go f x1 xs
                    | otherwise = go f x1 xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x xs
            where
                go f c [] = c
                go f c (x1:xs)
                    | f c x1 == LT = go f c xs
                    | f c x1 == GT = go f x1 xs
                    | otherwise = go f x1 xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare