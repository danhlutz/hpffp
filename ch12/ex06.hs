-- ex06.hs
module Exercise where

-- 1)
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left x):xs) = x : (lefts' xs)
lefts' ((Right y):ys) = lefts' ys

-- 2)
rights' :: [Either a b] -> [b]
rights' [] = []
rights' ((Right x):xs) = x : (rights' xs)
rights' (_:ys) = rights' ys

-- 3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' ((Left x):xs) = consLeft x (partitionEithers' xs)
partitionEithers' ((Right y):ys) = consRight y (partitionEithers' ys)

consLeft :: a -> ([a], [b]) -> ([a], [b])
consLeft x (xs, ys) = (x:xs, ys)

consRight :: b -> ([a], [b]) -> ([a], [b])
consRight y (xs, ys) = (xs, y:ys)

partition :: [Either a b] -> ([a], [b])
partition xs = (lefts' xs, rights' xs)

-- 4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x) 

-- 5)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x =
  either' (\ x -> Nothing)
          (\ x -> Just (f x))
          x
