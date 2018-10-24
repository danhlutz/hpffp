-- ch15/ex05.hs
-- Semigroup exercises

module Exercises where

import Test.QuickCheck
import Data.Semigroup

main :: IO ()
main = do
  putStrLn "Testing Semigroup instances"
  putStrLn ""
  putStrLn "Testing Trivial implementation"
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  putStrLn ""
  putStrLn "Testing Identity a implementation"
  quickCheck (semigroupAssoc :: IdAssoc)
  putStrLn ""
  putStrLn "Testing Two a b implementation"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn ""
  putStrLn "Testing Three a b c"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn ""
  putStrLn "Testing Four a b c d"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn ""
  putStrLn "Testing BoolConj"
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  putStrLn ""
  putStrLn "Testing BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn ""
  putStrLn "Testing Or a b"
  quickCheck (semigroupAssoc :: OrAssoc)

-- associativity test
semigroupAssoc :: (Semigroup a, Eq a) 
             => a -> a -> a -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

-- 1) Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

-- 2) 
newtype Identity a = Identity a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = idGen

idGen :: Arbitrary a => Gen (Identity a)
idGen = do
  x <- arbitrary
  return (Identity x)

instance Semigroup (Identity a) where
  x <> _ = x

type IdAssoc =  Identity String
             -> Identity String
             -> Identity String
             -> Bool

-- 3) a product type
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b)
       => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance Semigroup a => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' _) = Two (x <> x') y

type TwoAssoc =  Two (Sum Int) (Product Int)
              -> Two (Sum Int) (Product Int)
              -> Two (Sum Int) (Product Int)
              -> Bool

-- 4) a bigger product! 
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (<>) (Three x y z) (Three x' y' z') =
      (Three (x <> x') (y <> y') (z <> z'))

type BigThree = Three String (Sum Integer) (Product Integer)

type ThreeAssoc =  BigThree -> BigThree -> BigThree -> Bool

-- 5) even bigger!
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
         Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
        => Gen (Four a b c d)
fourGen = do
  h <- arbitrary
  i <- arbitrary
  j <- arbitrary
  k <- arbitrary
  return (Four h i j k)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (<>) (Four h i j k) (Four h' i' j' k') =
      (Four (h <> h') (i <> i') (j <> j') (k <> k'))

type BigFour = Four String (Sum Integer) String (Product Int)

type FourAssoc = BigFour -> BigFour -> BigFour -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = boolconjGen

boolconjGen :: Gen BoolConj
boolconjGen = do
  x <- arbitrary
  return (BoolConj x)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

boolDisjGen :: Gen BoolDisj
boolDisjGen = do
  x <- arbitrary
  return $ BoolDisj x

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = orGen

orGen :: (Arbitrary a, Arbitrary b)
      => Gen (Or a b)
orGen = do
  x <- arbitrary
  y <- arbitrary
  oneof [ return $ Fst x
        , return $ Snd y ]

instance Semigroup (Or a b) where
  (<>) (Snd x) _ = (Snd x)
  (<>) _ (Snd x) = (Snd x)
  (<>) _ x       = x

type OrAssoc =  Or String Int
             -> Or String Int
             -> Or String Int
             -> Bool

-- 9
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) =>
         Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) =
      Combine (\ x -> (f x) <> (g x))
