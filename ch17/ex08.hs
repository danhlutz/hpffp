-- ch17/ex08.hs

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Testing Pair a"
  quickBatch $ applicative appPair
  putStrLn ""
  putStrLn "Testing Two a b"
  quickBatch $ applicative twoCheck
  putStrLn ""
  putStrLn "Testing Three a b c"
  quickBatch $ applicative threeCheck
  putStrLn ""
  putStrLn "Testing Three' a b"
  quickBatch $ applicative threeCheck'
  putStrLn ""
  putStrLn "Testing Four a b c d"
  quickBatch $ applicative fourCheck
  putStrLn ""
  putStrLn "Testing Four' a b"
  quickBatch $ applicative fourCheck'

-- data declarations

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

appPair :: Pair (Int, Int, Int)
appPair = Pair (1, 2, 3) (1, 2, 3)

-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x 
  (<*>) (Two x g) (Two x' y) = Two (mappend x x') (g y)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

twoCheck :: Two (String, String, String) (String, String, String)
twoCheck = Two ("fun", "stuff", "here") ("Where", "is", "it")

--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) =>
         Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three x y f) (Three x' y' z') =
    Three (mappend x x') (mappend y y') (f z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type ThreeStr = (String, String, String)

threeCheck :: Three ThreeStr ThreeStr (Int, Int, Int)
threeCheck = Three ("fun", "stuff", "here")
                   ("where", "it's", "at")
                   (1, 2, 3)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f g) (Three' m n o) =
    Three' (mappend x m) (f n) (g o)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

threeCheck' :: Three' ThreeStr (Int, Int, Int)
threeCheck' = Three' ("fun", "stuff", "here")
                     (0, 1, 2)
                     (2, 3, 4)

-- 5)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four h i j k) = Four h i j (f k)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four h i j f) (Four m n o p) =
    Four (mappend h m) (mappend i n) (mappend j o) (f p)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)=>
         Arbitrary (Four a b c d) where
  arbitrary = do
    h <- arbitrary
    i <- arbitrary
    j <- arbitrary
    k <- arbitrary
    return $ Four h i j k

instance (Eq a, Eq b, Eq c, Eq d) =>
         EqProp (Four a b c d) where
  (=-=) = eq

three :: ThreeStr
three = ("fun", "stuff", "today")

fourCheck :: Four ThreeStr ThreeStr ThreeStr ThreeStr
fourCheck = Four three three three three

-- 6)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' h i j k) = Four' h i j (f k)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' x y z f) (Four' h i j k) =
    Four' (mappend x h) (mappend y i) (mappend z j) (f k)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    h <- arbitrary
    i <- arbitrary
    j <- arbitrary
    k <- arbitrary
    return $ Four' h i j k

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

fourCheck' :: Four' ThreeStr (Int, Int, Int)
fourCheck' = Four' three three three (1, 2, 3)
