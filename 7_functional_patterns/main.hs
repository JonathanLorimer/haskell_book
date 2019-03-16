module Ch7 where

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z


-- These have to be the same type because -- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

f :: (a, b, c) 
  -> (d, e, f) 
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

numbers x
    | x < 0  = -1
    | x == 0 = 0 |x>0 =1


add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO () 
main = do
    print (0 :: Int)
    print (add 1 0)
    print (addOne 0)
    print (addOnePF 0)
    print ((addOne . addOne) 0)
    print ((addOnePF . addOne) 0)
    print ((addOne . addOnePF) 0)
    print ((addOnePF . addOnePF) 0)
    print (negate (addOne 0))
    print ((negate . addOne) 0)
    print ((addOne . addOne . addOne . negate . addOne) 0)

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where xLast = x `divMod` 10
          d = mod (fst xLast) 10


hunsDigit :: Integral a => a -> a
hunsDigit x = d
    where xLast = x `divMod` 100
          d = mod (fst xLast) 10

foldBool :: a -> a -> Bool -> a
foldBool x y flag =
    case flag of
        True -> y
        False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y flag
    | flag = y
    | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)