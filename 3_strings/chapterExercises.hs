
-- | Question 1
-- a correct
-- b incorrect
-- c correct
-- d incorrect
-- e incorrect
-- f correct
-- g incorrect
-- h incorrect

-- | Question 2
-- a) concat [[1 * 6], [2 * 6], [3 * 6]] 
-- d) [6,12,18]

-- b) "rain" ++ drop 2 "elbow"
-- c) "rainbow" 

-- c) 10 * head [1, 2, 3]
-- e) 10

-- d) (take 3 "Julie") ++ (tail "yes")
-- a) "Jules"

-- e) concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]
-- b) [2,3,5,6,8,9]

-- | Question 3

-- a)
exclaim :: String -> String
exclaim x = x ++ "!"

-- b)
fifthChar :: String -> Char
fifthChar = head . drop 4

-- c)
lastWord :: String -> String
lastWord = last . words

-- | Question 4
thirdChar :: [a] -> a
thirdChar = flip (!!) 2

letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome!"




