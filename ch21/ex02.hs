
Signed-off-by: danhlutz <danhlutz@gmail.com>

-- ch21/ex02.hs

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a)
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a )
        => EqProp (S n a) where
  (S x y) =-= (S p q) =
        (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

sCheck :: S [] (Int, Int, [Int])
sCheck = S [(1, 2, [3])] (1, 2, [3])

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable (S n) where
  foldMap f (S _ y) = f y

instance (Functor n, Traversable n) => Traversable (S n) where
  traverse f (S x y) = S <$> (traverse f x) <*> f y

main :: IO ()
main = do
  putStrLn "Testing S n a"
  quickBatch $ traversable sCheck
