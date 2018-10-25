-- ch15/ex07.hs
-- Monoid instance exercises

module Exercises where

import Test.QuickCheck
import Data.Semigroup

main :: IO ()
main = do
  let sa  = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  putStrLn "Tests for Monoid instances"
  putStrLn ""
  putStrLn "Trivial tests"
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)
  putStrLn "Identity a tests"
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "Two a b tests"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  putStrLn "BoolConj tests"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "BoolDisj tests"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

-- associativity and identity rules
semigroupAssoc :: (Semigroup a, Eq a) 
               => a -> a -> a -> Bool
semigroupAssoc x y z =
 (x <> (y <> z)) == ((x <> y) <> z)

monoidLeftIdentity :: (Semigroup a, Monoid a, Eq a)
                   => a -> Bool
monoidLeftIdentity x =
  mempty <> x == x

monoidRightIdentity :: (Semigroup a, Monoid a, Eq a)
                    => a -> Bool
monoidRightIdentity x =
  x <> mempty == x

-- 1)
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = trivGen

trivGen :: Gen Trivial
trivGen = do
  return Trivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2)
data Identity a =
  Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = idGen

idGen :: Arbitrary a => Gen (Identity a)
idGen = do
  x <- arbitrary
  return $ Identity x

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = (Identity (x <> y))

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = (Identity mempty)
  mappend = (<>)

type IdAssoc = Identity String
             -> Identity String
             -> Identity String
             -> Bool

-- 3)
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

instance (Semigroup a, Semigroup b) =>
  Semigroup (Two a b) where
    (<>) (Two h i) (Two j k) = (Two (h <> j) (i <> k))

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) =>
  Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

type TwoAssoc =  Two String (Sum Int)
              -> Two String (Sum Int)
              -> Two String (Sum Int)
              -> Bool

-- 4)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

boolConjGen :: Gen BoolConj
boolConjGen = do
  x <- arbitrary
  return (BoolConj x)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

boolDisjGen :: Gen BoolDisj
boolDisjGen = do
  x <- arbitrary
  return (BoolDisj x)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
