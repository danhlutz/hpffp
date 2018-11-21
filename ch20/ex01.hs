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
-- 5 myMax

-- 6 myNull

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
