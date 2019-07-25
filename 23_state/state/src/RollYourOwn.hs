module RollYourOwn where

import RandomExample
import System.Random

myRollsToGetTwenty :: Int -> StdGen -> (Int, [Die])
myRollsToGetTwenty lim g = go 0 0 g []
    where
        go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
        go sum count gen rolls
            | sum >= lim = (count, rolls)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen ((intToDie die):rolls)
