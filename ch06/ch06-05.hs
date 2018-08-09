--ch06-05.hs
module MoreTypes where

--doesn't work
--add :: a -> a -> a
add :: Num a => a -> a -> a
add x y = x + y

add' :: Int -> Int -> Int
add' x y = x + y

-- doesn't work
-- addWeird :: Num a => a -> a -> a
addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x

addWeird' :: Int -> Int -> Int
addWeird' x y =
  if x > 1
  then x + y
  else x

check' :: Int -> Int -> Bool
check' a a' = a == a'
