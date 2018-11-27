-- ch21/ex01.hs
-- chapter exercises

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Testing Identity a"
  quickBatch (traversable identCheck)
  putStrLn ""
  putStrLn "Testing Optional a"
  quickBatch (traversable optCheck)
  putStrLn ""
  putStrLn "Testing List a"
  quickBatch (traversable listCheck)
  putStrLn "Testing Three a b c"
  quickBatch (traversable threeCheck)
  putStrLn "Testing Pair a b"
  quickBatch (traversable pairCheck)
  putStrLn "Testing Big a b"
  quickBatch (traversable bigCheck)
  putStrLn "Testing Bigger a b"
  quickBatch (functor biggerCheck)
  quickBatch (traversable biggerCheck)

-- 1) Identity

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identCheck :: Identity (Int, Int, [Int])
identCheck = Identity (1, 2, [3])

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

--

newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Constant a b) where
    arbitrary = do
      x <- arbitrary
      return (Constant x)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

--

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = maybeGen

maybeGen :: Arbitrary a => Gen (Optional a)
maybeGen = do
  x <- arbitrary
  frequency [(1, return Nada)
            ,(3, return $ Yep x)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

optCheck :: Optional (Int, Int, [Int])
optCheck = Yep (1, 2, [3])

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = Yep <$> (f x)

--

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

listToCons :: [a] -> List a
listToCons = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return (listToCons x)

listCheck :: List (Int, Int, [Int])
listCheck = Cons (1, 2, [3]) Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x y) = mappend (f x) (foldMap f y)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x y) = (Cons <$> f x) <*> (traverse f y)

--

data Three a b c =
  Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three x y z

instance (Eq a, Eq b, Eq c) =>
  EqProp (Three a b c) where
    (=-=) = eq

threeCheck :: Three Int Int (Int, Int, [Int])
threeCheck = Three 1 2 (1, 2, [3])

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> (f z)

--

data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Pair a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return $ Pair x y

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

pairCheck :: Pair Int (Int, Int, [Int])
pairCheck = Pair 0 (1, 2, [3])

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = (Pair x) <$> (f y)

--

data Big a b = Big a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Big a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Big x y z

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

bigCheck :: Big Int (Int, Int, [Int])
bigCheck = undefined

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ y z) = mappend (f y) (f z)

instance Traversable (Big a) where
  traverse f (Big x y z) = (Big x) <$> (f y) <*> (f z)

--

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Bigger a b) where
    arbitrary = do
      h <- arbitrary
      i <- arbitrary
      j <- arbitrary
      k <- arbitrary
      return $ Bigger h i j k

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

biggerCheck :: Bigger Int (Int, Int, [Int])
biggerCheck = undefined

instance Functor (Bigger a) where
  fmap f (Bigger h i j k) = Bigger h (f i) (f j) (f k)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ i j k) = mappend (f i) (mappend (f j) (f k))

instance Traversable (Bigger a) where
  traverse f (Bigger h i j k) =
    (Bigger h) <$> (f i) <*> (f j) <*> (f k)
