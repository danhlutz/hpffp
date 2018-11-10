-- ch17/ex07.hs

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = quickBatch $ applicative abc

-- test cases

abc :: Validation [Errors] (Int, Int, Int)
abc = Success' (1, 2, 3)

success :: Validation [Errors] Int
success = Success' (+1) <*> Success' 1

failure :: Validation [Errors] Int
failure = Success' (+1) <*> Failure' [StackOverflow]

failure' :: Validation [Errors] Int
failure' = Failure' [StackOverflow] <*> Success' (+ (1 :: Int))

failures :: Validation [Errors] Int
failures = Failure' [MooglesChewedWires] <*> Failure' [StackOverflow]

--

data Validation err a =
    Failure' err
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success' x) = Success' (f x)
  fmap _ (Failure' x) = Failure' x

instance Monoid e =>
         Applicative (Validation e) where
  pure x = Success' x
  (<*>) (Failure' x) (Failure' y) = Failure' (mappend x y)
  (<*>) (Failure' x) _ = Failure' x
  (<*>) _ (Failure' x) = Failure' x
  (<*>) (Success' f) (Success' x) = Success' (f x)

instance (Arbitrary e, Arbitrary a) =>
  Arbitrary (Validation e a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      frequency [ (1, return $ Failure' y)
                , (3, return $ Success' x)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Arbitrary Errors where
  arbitrary =
    frequency [ (1, return DividedByZero)
              , (1, return StackOverflow)
              , (1, return MooglesChewedWires)]
