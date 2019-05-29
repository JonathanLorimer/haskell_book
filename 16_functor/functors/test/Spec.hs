import           FunctorInstances
import           Test.Hspec
import           Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
    putStrLn "\nExecuting Property Tests"
    putStrLn "Functor Laws for: Lists"
    quickCheck (functorCompose (+ 1) (* 2) :: [Int] -> Bool)
    quickCheck (functorIdentity :: [Int] -> Bool)
    quickCheck (functorCompose (const 'c') (const 'd') :: String -> Bool)
    quickCheck (functorIdentity :: String -> Bool)
    putStrLn "Functor Laws for: Identity"
    quickCheck (functorCompose (+ 1) (* 2) :: Identity Int -> Bool)
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose (const 'c') (const 'd') :: Identity Char -> Bool)
    quickCheck (functorIdentity :: Identity Char -> Bool)
    putStrLn "Functor Laws for: Pair"
    quickCheck (functorCompose (+ 1) (* 2) :: Pair Int -> Bool)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose (const 'c') (const 'd') :: Pair Char -> Bool)
    quickCheck (functorIdentity :: Pair Char -> Bool)
    putStrLn "Functor Laws for: Two"
    quickCheck (functorCompose (+ 1) (* 2) :: Two Int Int -> Bool)
    quickCheck (functorIdentity :: Two Bool Int -> Bool)
    quickCheck
        (functorCompose (const 'c') (const 'd') :: Two String Int -> Bool)
    quickCheck (functorIdentity :: Two Bool Char -> Bool)
    putStrLn "Functor Laws for: Three"
    quickCheck (functorCompose (+ 1) (* 2) :: Three Int Int Int -> Bool)
    quickCheck (functorIdentity :: Three Bool Int Int -> Bool)
    quickCheck
        (functorCompose (const 'c') (const 'd') :: Three String Int Char -> Bool
        )
    quickCheck (functorIdentity :: Three Bool Char Bool -> Bool)
    putStrLn "Functor Laws for: Three'"
    quickCheck (functorCompose (+ 1) (* 2) :: Three' Int Int -> Bool)
    quickCheck (functorIdentity :: Three' Bool Int -> Bool)
    quickCheck
        (functorCompose (const 'c') (const 'd') :: Three' String Char -> Bool)
    quickCheck (functorIdentity :: Three' Bool Char -> Bool)
    putStrLn "Functor Laws for: Four"
    quickCheck (functorCompose (+ 1) (* 2) :: Four Int Int Int Int -> Bool)
    quickCheck (functorIdentity :: Four Bool Int Int Char -> Bool)
    quickCheck
        (functorCompose (const 'c') (const 'd') :: Four String Int Char Char
          -> Bool
        )
    quickCheck (functorIdentity :: Four Bool Char Bool Char -> Bool)
    putStrLn "Functor Laws for: Four'"
    quickCheck (functorCompose (+ 1) (* 2) :: Four' Int Int Int -> Bool)
    quickCheck (functorIdentity :: Four' Bool Int Char -> Bool)
    quickCheck
        (functorCompose (const 'c') (const 'd') :: Four' String Int Char -> Bool)
    quickCheck (functorIdentity :: Four' Bool Bool Char -> Bool)
