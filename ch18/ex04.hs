-- ch18/ex04.hs
-- Chapter exercises 18.7

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Testing Nope a"
  quickBatch $ functor nopeTest
  quickBatch $ applicative nopeTest
  quickBatch $ applicative nopeTest
  putStrLn ""
  putStrLn "Testing FlipEither b a"
  quickBatch $ functor flipCheck
  quickBatch $ applicative flipCheck
  quickBatch $ monad flipCheck
  putStrLn ""
  putStrLn "Testing Identity a"
  quickBatch $ functor identCheck
  quickBatch $ applicative identCheck
  quickBatch $ monad identCheck
  putStrLn ""
  putStrLn "Testing List a"
  quickBatch $ monoid (Cons "fun" Nil)
  quickBatch $ functor listCheck
  quickBatch $ applicative listCheck
  quickBatch $ monad listCheck

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = oneof [return NopeDotJpg]

nopeTest :: Nope (Int, Int, Int)
nopeTest = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _  = NopeDotJpg

-- 2
data FlipEither b a =
    Lefty a
  | Righty b
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (FlipEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (FlipEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [ return $ Lefty x
          , return $ Righty y ]

flipCheck :: FlipEither String (Int, Int, Int)
flipCheck = Lefty (1, 2, 3)

instance Functor (FlipEither b) where
  fmap f (Lefty x)  = Lefty (f x)
  fmap _ (Righty y) = Righty y

instance Applicative (FlipEither b) where
  pure x = Lefty x
  (<*>) (Lefty f) z  = fmap f z
  (<*>) (Righty x) _ = Righty x

instance Monad (FlipEither b) where
  return x = Lefty x
  Lefty x >>= f  = f x
  Righty y >>= _ = Righty y

-- 3
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

identCheck :: Identity (Int, Int, Int)
identCheck = Identity (1, 2, 3)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> z = fmap f z

instance Monad Identity where
  return x = Identity x
  Identity x >>= f = f x

-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: List a -> Int -> List a
take' Nil _ = Nil
take' _ 0   = Nil
take' (Cons x y) n = Cons x (take' y (n - 1))

listToList :: [a] -> List a
listToList = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return $ take' (listToList x) 3000

listCheck :: List (Int, Int, Int)
listCheck = Cons (1, 2, 3) Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x y) z = Cons x (mappend y z)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f g) z =
    mappend (fmap f z) (g <*> z)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x y) f = mappend (f x) (y >>= f)
