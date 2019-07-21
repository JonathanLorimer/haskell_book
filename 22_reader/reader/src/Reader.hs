module Reader where

import Control.Applicative

times2 = (*2)
plus10 = (+10)

comp :: Integer -> Integer 
comp = times2 . plus10

appl :: Integer -> Integer
appl = (+) <$> times2 <*> plus10

appl' :: Integer -> Integer
appl' = liftA2 (+) times2 plus10

doAdd :: Integer -> Integer
doAdd = do
    t2 <- times2
    p10 <- plus10
    return (t2 + p10)

bindAdd :: Integer -> Integer
bindAdd = plus10 <$> times2 >>= (+)
