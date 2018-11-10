-- ch17/ex06.hs
-- ZipList applicative

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- tests

main :: IO ()
main = do
  quickBatch $ applicative testZL

testZL = ZipList' (Cons (1 :: Int, 1 :: Int, 1 :: Int) Nil)

-- implementation

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x y) = Cons x (take' (n - 1) y)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x y) z = Cons x (mappend y z)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f h) x =
    mappend (fmap f x) (h <*> x)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = conGen

conGen :: Arbitrary a => Gen (List a)
conGen = do
  x <- listGen
  return $ foldr Cons Nil x

listGen :: Arbitrary a => Gen [a]
listGen = do
  x <- arbitrary
  return x

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (pure x)
  (<*>) x (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) x = ZipList' Nil
  (<*>) (ZipList' fs) (ZipList' xs) =
    ZipList' $ combine fs xs

combine :: List (a -> b) -> List a -> List b
combine Nil _ = Nil
combine _ Nil = Nil
combine (Cons f fs) (Cons x xs) = Cons (f x) (combine fs xs) 

mkZL :: [a] -> ZipList' a
mkZL xs = ZipList' (foldr Cons Nil xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = zlGen

zlGen :: Arbitrary a => Gen (ZipList' a)
zlGen = do
  x <- conGen
  return $ ZipList' x
