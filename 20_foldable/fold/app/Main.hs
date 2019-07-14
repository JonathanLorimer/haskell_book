module Main where

import qualified FoldFunctions as F
import ChapterExercises

main :: IO ()
main = do
    print $ F.sum [1,2,3,4,5]
    print $ Constant 1
    return ()
