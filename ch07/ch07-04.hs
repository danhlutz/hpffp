-- ch07-04.hs
module ChapPractice where

funcZ :: (Num a, Eq a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "wut" 

pal :: Eq a => [a] -> [Char]
pal xs =
  case xs == reverse xs of
    True  -> "yes"
    False -> "no"

pal' :: Eq a => [a] -> [Char]
pal' xs =
  case y of
    True  -> "yes"
    False -> "no"
  where y = xs == reverse xs
