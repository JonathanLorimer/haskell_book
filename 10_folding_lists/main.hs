
import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    ]

dbDateToList :: DatabaseItem -> [UTCTime] -> [UTCTime]
dbDateToList (DbDate time) xs = time:xs
dbDateToList _ xs = xs

maxDbDate :: DatabaseItem -> UTCTime -> UTCTime
maxDbDate (DbDate time1) time2 = max time1 time2
maxDbDate _ time = time

dbNumberToList :: DatabaseItem -> [Integer] -> [Integer]
dbNumberToList (DbNumber num) xs = num:xs
dbNumberToList _ xs = xs

sumDbNumber :: DatabaseItem -> Integer -> Integer
sumDbNumber (DbNumber num1) num2 = num1 + num2
sumDbNumber _ num = num

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr dbDateToList []

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldl (flip dbDateToList) []
    
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr dbNumberToList []
    
filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = foldl (flip dbNumberToList) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr maxDbDate (UTCTime
                                (fromGregorian 0 0 0)
                                (secondsToDiffTime 34123)
                             )


sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumDbNumber 0

sumAndLength :: DatabaseItem -> (Integer, Integer) -> (Integer, Integer)
sumAndLength (DbNumber num) (sum, length) = (num + sum, length + 1)
sumAndLength _ sumLength = sumLength

divTuple :: (Integer, Integer) -> Double
divTuple (x,y) = (fromIntegral x) / (fromIntegral y)

avgDb :: [DatabaseItem] -> Double
avgDb = divTuple . foldr sumAndLength (0,0)

fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs

factorial = scanl (+) 1 [2..] 

-- Chapter Exercises

stops  = "pbtdkg"
vowels = "aeiouf"

nouns = ["bird", "dog", "kitten", "coffee", "girlfriend", "desk"]
verbs = ["walk", "attack", "eat", "study", "break"]

permutate a b = (,,) <$> a <*> b <*> a
-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combina- tions. These will not all correspond to real words in English,although the stop-vowel-stop pattern is common enough that many of them will.
-- b) Modify that function so that it only returns the combina-tions that begin with a p.
-- c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples represent- ing possible noun-verb-noun sentences.
-- 2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.
-- seekritFunc x =
-- div (sum (map length (words x)))
--            (length (words x))
-- 3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?

seekritFunc :: String -> Double
seekritFunc x =
    ((/) . fromIntegral . sum . map length . words $ x) $ (fromIntegral . length . words) x

-- takes the number of words in the string and divides it by the sum of the length of the individual words

myOr :: [Bool] -> Bool
myOr = foldr pred False
    where
        pred _ True = True
        pred True _ = True
        pred False _ = False


myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr applyPred False
        where
            applyPred _ True = True
            applyPred item _ = pred item


myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr applyPred False 
    where
        applyPred _ True = True
        applyPred item _ = (== x) item

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr applyPred [] 
    where
        applyPred item acc
            | pred item = item:acc
            | otherwise = acc


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr applyPred x xs
    where
        applyPred item acc
            | f item acc == GT = item
            | f item acc == LT = acc
            | otherwise = item

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr applyPred x xs
    where
        applyPred item acc
            | f item acc == LT = item
            | f item acc == GT = acc
            | otherwise = item
            

