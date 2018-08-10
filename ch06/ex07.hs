--ex07.hs
module Substitute where

import Data.List (sort)

-- 2)
f2 :: Float
f2 = 1.0

-- will not work
-- f2' :: Num a => a
-- f2' = 1.0

-- 3)
f3 :: Float
f3 = 1.0

f3' :: Fractional a => a
f3' = 1.0

-- 4)
f4 :: Float
f4 = 1.0

-- this should work!
f4' :: RealFrac a => a
f4' = 1.0

-- 5)
freud :: a -> a
freud x = x

-- this should work!
freud' :: Ord a => a -> a
freud' x = x

-- 6)
freud6 :: a -> a
freud6 x = x

-- this should work!
freud6' :: Int -> Int
freud6' x = x

-- 7)
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- this will not work because myX is of concrete type Int
-- sigmund' :: a -> a
-- sigmund' x = myX

-- 8)
sigmund8 :: Int -> Int
sigmund8 x = myX

-- this is not going to work since sigmund8' yields a value of definite
-- type Int
-- sigmund8' :: Num a => a -> a
-- sigmund8' x = myX

-- 9)
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- this will typecheck
jung' :: [Int] -> Int
jung' xs = head (sort xs)

-- 10)
young :: [Char] -> Char
young xs = head (sort xs)

-- this will typecheck because sort only requires 
-- an input :: Ord a => [a]
young' :: Ord a => [a] -> a
young' xs = head (sort xs)

-- 11)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- this will not typecheck because mySort has the concrete type
-- [Char] -> [Char]
-- signifier' :: Ord a => [a] -> a
-- signifier' xs = head (mySort xs)
