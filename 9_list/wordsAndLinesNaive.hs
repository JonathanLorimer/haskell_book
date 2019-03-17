myWords :: String -> [String]
myWords = go []
    where
        go xs [] = reverse xs
        go xs string = go ((extractWord string):xs) $ removeWordAndSpace string

extractWord :: String -> String
extractWord = takeWhile (/= ' ')

removeWordAndSpace :: String -> String
removeWordAndSpace = (drop 1) . dropWhile (/= ' ')

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = go []
    where
        go xs [] = reverse xs
        go xs string = go ((extractLine string):xs) $ removeLineAndNewline string

extractLine :: String -> String
extractLine = takeWhile (/= '\n')

removeLineAndNewline :: String -> String
removeLineAndNewline = (drop 1) . dropWhile (/= '\n')