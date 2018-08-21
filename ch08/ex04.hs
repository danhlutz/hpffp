-- ex04.hs
module RecursionExercises where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

-- 1) write out the steps of dividedBy 15 2
--    dividedBy 15 2
--    go 15 2 0   -- case otherwise
--    go 13 2 1   -- case otherwise
--    go 11 2 2   -- case otherwise
--    go  9 2 3   -- case otherwise
--    go  7 2 4   -- case otherwise
--    go  5 2 5   -- case otherwise
--    go  3 2 6   -- case otherwise
--    go  1 2 7   -- base case
--    (7, 1)

-- 2)
sumInts :: (Eq a, Num a) => a -> a
sumInts 1 = 1
sumInts x = x + sumInts (x - 1)

-- 2 using tail recursion
sumInts' :: (Eq a, Num a) => a -> a
sumInts' x = go x 0
  where go n sum
         | n == 0    = sum
         | otherwise =
             go (n - 1) (sum + n)

-- 3)
myMult :: (Integral a) => a -> a -> a
myMult x y = go x y 0
  where go a b product
         | b == 0    = product
         | otherwise =
             go a (b - 1) (product + a)
