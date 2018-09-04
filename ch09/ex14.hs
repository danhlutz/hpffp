-- ex14.hs
module MyStandards where

-- 1)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f []      = False
myAny f (x:xs)
  | f x == True = True
  | otherwise   = myAny f xs

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem x []     = False
myElem x (y:ys)
  | x == y    = True
  | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = any same ys
  where same a = a == x 

-- 4)
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 5)
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

-- 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

-- 7)
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = go x xs
  where go tmpMax [] = tmpMax
        go tmpMax (y:ys)
           | (comp tmpMax y) == GT = go tmpMax ys
           | otherwise             = go y      ys

-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = go x xs
  where go tmpMin [] = tmpMin
        go tmpMin (y:ys)
           | (comp tmpMin y) == LT = go tmpMin ys
           | otherwise             = go y      ys

-- 10)
myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs
