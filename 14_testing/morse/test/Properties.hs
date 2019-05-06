module Properties where
import           Test.QuickCheck
import           Data.List                      ( sort )

main :: IO ()
main = do
    putStrLn "prop_halfIdentity"
    quickCheck prop_halfIdentity
    putStrLn "prop_sortedList [Int]"
    quickCheck (prop_sortedList :: [Int] -> Bool)
    putStrLn "prop_sortedList [String]"
    quickCheck (prop_sortedList :: [String] -> Bool)
    putStrLn "prop_sortedList [Ordering]"
    quickCheck (prop_sortedList :: [Ordering] -> Bool)
    putStrLn "prop_sortedList [Bool]"
    quickCheck (prop_sortedList :: [Bool] -> Bool)
    putStrLn "plusAssociative"
    quickCheck plusAssociative
    putStrLn "plusCommutative"
    quickCheck plusCommutative
    putStrLn "mulAssociative"
    quickCheck mulAssociative
    putStrLn "mulCommutative"
    quickCheck mulCommutative
    putStrLn "prop_quotPlusRem"
    quickCheck prop_quotPlusRem
    putStrLn "prop_divPlusMod"
    quickCheck prop_divPlusMod

-- | Half Property
half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_halfIdentity x = halfIdentity x == x

-- | Ordered List
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_      , False) = status
    go y (       Nothing, t    ) = (Just y, t)
    go y (       Just x , t    ) = (Just y, x >= y)

prop_sortedList :: (Ord a) => [a] -> Bool
prop_sortedList = listOrdered . sort

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

mulAssociative :: Int -> Int -> Int -> Bool
mulAssociative x y z = x * (y * z) == (x * y) * z

mulCommutative :: Int -> Int -> Bool
mulCommutative x y = x * y == y * x

prop_quotPlusRem :: Int -> Int -> Bool
prop_quotPlusRem x y = (quot x y) * y + (rem x y) == x

prop_divPlusMod :: Int -> Int -> Bool
prop_divPlusMod x y = (div x y) * y + (mod x y) == x
