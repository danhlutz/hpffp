-- ex06.hs
module MyFuncs where

-- 1)
-- using direct recursion
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- point-free
myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

-- 2)
-- using direct recursion
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = (f x) || myAny f xs

-- using a fold
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs = foldr (||) False (map f xs)

-- point-free
-- ???

-- 3)
-- direct recursion
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ []     = False
myElem1 x (y:ys)
  | x == y    = True
  | otherwise = myElem1 x ys

-- using a fold
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x ys = foldr (||) False (map (==x) ys)

-- using any
myElem3 :: Eq a => a -> [a] -> Bool
myElem3 x = any (==x)

-- 4)
-- direct recursion
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (++) (myReverse xs) [x]

-- using a fold, point-free
myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

-- 5) 
-- direct recursion
myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = (f x) : (myMap f xs)

-- using a fold
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\ x xs -> f x : xs) []

-- 6)
-- direct recursion
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
  | f x == True = x : myFilter f xs
  | otherwise   = myFilter f xs

-- using a fold
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\x xs ->
                      if f x == True
                      then x : xs
                      else xs) []

-- 7)
-- direct recursion
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = (++) x (squish xs)

-- using a fold, point-free
squish2 :: [[a]] -> [a]
squish2 = foldr (++) []

-- 8)
-- using direct recursion
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

-- using a fold
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f = foldr (\x xs -> (f x) ++ xs) []

-- 9) 
squishAgain :: [[a]] -> [a]
squishAgain = squishMap2 id

-- 10)
-- direct recursion
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy c (x:xs) = go x xs
  where go tmpMax [] = tmpMax
        go tmpMax (y:ys)
           | c tmpMax y == GT = go tmpMax ys
           | otherwise        = go y ys

-- using a fold
myMaxBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaxBy2 c (x:xs) = foldr check x xs
  where check a b
          | c a b == GT = a
          | otherwise   = b

-- 11)
-- direct recursion
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy c (x:xs) = go x xs
  where go tmpMin [] = tmpMin
        go tmpMin (y:ys)
           | c tmpMin y == LT = go tmpMin ys
           | otherwise        = go y ys

-- using a fold
myMinBy2 :: (a -> a -> Ordering) -> [a] -> a
myMinBy2 c (x:xs) = foldr check x xs
  where check a b
          | c a b == LT = a
          | otherwise   = b
