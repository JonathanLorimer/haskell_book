module SkiFree where
    
import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (f <$> na) (f a)

instance Foldable (S n) where
    foldr f z (S na a) = f a z

instance ( Functor n
    , Arbitrary (n a)
    , Arbitrary a )
    => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
    , Testable (n Property)
    , Eq a
    , Eq (n a) , EqProp a)
    => EqProp (S n a) where (=-=) = eq

instance Traversable n
    => Traversable (S n) where
    traverse f (S na a) = S <$> (traverse f na) <*> (f a)

-- main = sample' (arbitrary :: Gen (S [] Int))