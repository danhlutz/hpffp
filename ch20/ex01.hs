-- ch20/ex01.hs

module Exercise where

import Data.Monoid

mySum :: (Foldable t, Num a) => t a -> a
mySum = foldr (+) 0

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = foldr (*) 1

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x xs = getAny (foldMap (\n -> Any (n == x)) xs)

-- 4 myMin

data MyMin a =
    Minny
  | MyMin a
  deriving (Eq, Show)

instance Ord a => Monoid (MyMin a) where
  mempty = Minny
  mappend Minny x = x
  mappend x Minny = x
  mappend (MyMin x) (MyMin y) =
    if x < y then (MyMin x) else (MyMin y)

getMyMin :: MyMin a -> Maybe a
getMyMin Minny = Nothing
getMyMin (MyMin x) = Just x

myMin :: (Foldable t, Ord a) => t a -> Maybe a
myMin xs = getMyMin $ foldMap MyMin xs

-- 5 myMax

data MyMax a =
    Maxxy
  | MyMax a
  deriving (Eq, Show)

instance Ord a => Monoid (MyMax a) where
  mempty = Maxxy
  mappend Maxxy x = x
  mappend x Maxxy = x
  mappend (MyMax x) (MyMax y) =
    if x > y then (MyMax x) else (MyMax y)

getMyMax :: MyMax a -> Maybe a
getMyMax Maxxy = Nothing
getMyMax (MyMax x) = Just x

myMax :: (Foldable t, Ord a) => t a -> Maybe a
myMax xs = getMyMax $ foldMap MyMax xs

-- 6 myNull

data MyNull a =
    Nullity
  | NotANull a
  deriving (Eq, Show)

instance Monoid (MyNull a) where
  mempty = Nullity
  mappend Nullity x = x
  mappend x _ = x

myNull :: (Foldable t) => t a -> Bool
myNull xs = isNull $ foldMap NotANull xs

isNull :: MyNull a -> Bool
isNull Nullity = True
isNull _ = False

myLen :: (Foldable t) => t a -> Int
myLen xs = foldr (\_ n -> n + 1) 0 xs

myToList :: (Foldable t) => t a -> [a]
myToList = foldMap (\n -> [n])

myFold :: (Foldable t, Monoid m) => t m -> m
myFold xs = foldMap id xs

myFoldMap :: (Foldable t, Monoid m)
          => (a -> m) -> t a -> m
myFoldMap f xs = foldr combo mempty xs
  where combo x y = f x <> y
