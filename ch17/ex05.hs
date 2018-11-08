-- ch17/ex05.hs

module Exercise where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  quickBatch $ applicative (Cons (1 :: Int, 2 :: Int, True) Nil)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

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
    mappend (fmap f x) ((<*>) h x)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = conGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

conGen :: Arbitrary a => Gen (List a)
conGen = do
  x <- listGen
  return $ foldr Cons Nil x

listGen :: Arbitrary a => Gen [a]
listGen = do
  x <- arbitrary
  return x
