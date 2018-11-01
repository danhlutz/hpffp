-- ch16/ex08.hs
-- chapter exercises

module Exercise where

import GHC.Arr

-- can a valid functor instance be written?

-- 1) Bool: No, Functor. Bool is type *. The type constructor needs
--    at least one type argument

-- 2)
data BoolAndSomething a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomething where
  fmap f (False' x) = False' (f x)
  fmap g (True' y) = True' (g y)

-- 3)
data BoolAndMaybeSomething a =
  Falsish | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomething where
  fmap f (Truish x) = Truish (f x)
  fmap _ _ = Falsish

-- 5)
data D = D (Array Word Word) Int Int
-- this needs a type argument in order to have a functor instance
