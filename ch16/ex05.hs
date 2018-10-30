-- ch16/ex05.hs
{-# LANGUAGE ViewPatterns #-}

module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = do
  putStrLn "Test Functor instances"
  putStrLn ""
  putStrLn "Testing Identity"
  quickCheck (fIdentity :: Identity Int -> Bool)
  quickCheck (fCompose :: IdFC)
  putStrLn ""
  putStrLn "Testing Pair a"
  quickCheck (fIdentity :: Pair String -> Bool)
  quickCheck (fCompose :: PairID)
  putStrLn ""
  putStrLn "Testing Two a b"
  quickCheck (fIdentity :: Two String String -> Bool)
  quickCheck (fCompose :: TwoComp)
  putStrLn ""
  putStrLn "Testing Three a b c"
  quickCheck (fIdentity :: Three String Bool Bool -> Bool)
  quickCheck (fCompose :: ThreeComp)
  putStrLn ""
  putStrLn "Testing Three' a b"
  quickCheck (fIdentity :: Three' Bool String -> Bool)
  quickCheck (fCompose :: ThreePComp)
  putStrLn ""
  putStrLn "Testing Four a b c d"
  quickCheck (fIdentity :: Four String String Bool Int -> Bool)
  quickCheck (fCompose :: FourComp)
  putStrLn ""
  putStrLn "Testing Four' a b"
  quickCheck (fIdentity :: Four' String Bool -> Bool)
  quickCheck (fCompose :: FourPComp)

-- Functor laws
fIdentity :: (Functor f, Eq (f a))
          => f a -> Bool
fIdentity f = fmap id f == f

fCompose :: (Eq (f c), Functor f)
         => f a -> Fun a b -> Fun b c -> Bool
fCompose x (Fun _ f) (Fun _ g) =
  (fmap (g .f) x) == (fmap g . fmap f $ x)

-- 1)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = idGen

idGen :: Arbitrary a => Gen (Identity a)
idGen = do
  x <- arbitrary
  return (Identity x)

type IntToInt = Fun Int Int

type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool

-- 2)
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: (Arbitrary a) => Gen (Pair a)
pairGen = do
  x <- arbitrary
  y <- arbitrary
  return (Pair x y)

type StrToStr = Fun String String

type PairID = Pair String -> StrToStr -> StrToStr -> Bool

-- 3)
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b)
       => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

type TwoComp = Two String Int -> IntToInt -> IntToInt -> Bool

-- 4)
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three m n o) = Three m n (f o)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

type ThreeComp = Three String Bool Int -> IntToInt -> IntToInt -> Bool

-- 5)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Three' a b) where
    arbitrary = threePGen

threePGen :: (Arbitrary a, Arbitrary b)
          => Gen (Three' a b)
threePGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three' x y z)

type ThreePComp = Three String Int Int -> IntToInt -> IntToInt -> Bool

-- 6)
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
    arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
        => Gen (Four a b c d)
fourGen = do
  h <- arbitrary
  i <- arbitrary
  j <- arbitrary
  k <- arbitrary
  return (Four h i j k)

type FourComp =  Four String Bool Double Int
              -> IntToInt
              -> IntToInt
              -> Bool

-- 7)
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' h i j k) = Four' h i j (f k)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourPGen

fourPGen :: (Arbitrary a, Arbitrary b)
         => Gen (Four' a b)
fourPGen = do
  h <- arbitrary
  i <- arbitrary
  j <- arbitrary
  k <- arbitrary
  return (Four' h i j k)

type FourPComp = Four' String Int -> IntToInt -> IntToInt -> Bool

-- 8) It is not possible to write a Functor instance for
--    data Trivial = Trivial because Functor requires a type constructor
--    with the kind * -> *
