-- ch18/ex02.hs

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "These paws uphold the laws"
  quickBatch $ functor sumCheck
  quickBatch $ applicative sumCheck
  quickBatch $ monad sumCheck

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (First x) _  = First x
  (<*>) (Second f) z = fmap f z

instance Monad (Sum a) where
  return x = Second x
  First x  >>= _  = First x
  Second y >>= z  = z y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [(return $ First x), (return $ Second y)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

sumCheck :: Sum String (Int, Int, Int)
sumCheck = Second (1, 2, 3)
