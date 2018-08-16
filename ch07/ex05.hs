-- ex05.hs
module GuardPractice where

-- 1)
badAvg :: (Fractional a, Ord a)
       => a -> Char

badAvg x
   | otherwise = 'F'
   | y >= 0.9  = 'A'
   | y >= 0.8  = 'B'
   | y >= 0.7  = 'C'
   | y >= 0.59 = 'D'
   where y = x / 100

-- badAvg always returns 'F'

-- 2)
badAvg2 :: (Fractional a, Ord a) => a -> Char
badAvg2 x
   | y >= 0.7  = 'C'
   | y >= 0.9  = 'A'
   | y >= 0.8  = 'B'
   | y >= 0.59 = 'D'
   | otherwise = 'F'
   where y = x / 100

-- badAvg2 does not return 'A' or 'B' values because the guards 
-- are out of order

-- 3)
pal :: Eq a => [a] -> Bool
pal xs
   | xs == reverse xs = True
   | otherwise        = False

-- pal returns b) True when xs is a palindrome

-- 4) pal can take an argument of type Eq a => [a], basically any list
--    where the list items can be checked for equality

-- 5) pal :: Eq a => [a] -> Bool

-- 6)
numbers :: (Num a, Ord a, Num b) => a -> b
numbers x
   | x < 0  = -1
   | x == 0 =  0
   | x > 0  =  1

-- 7) numbers can take any argument that is an instance of
--    Num and Ord

-- 8) numbers :: (Num a, Ord a, Num b) => a -> b
