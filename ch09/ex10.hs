-- ex10.hs
module Mystery where

import Data.Bool (bool)

-- 4)
itIsMystery :: [Char] -> [Bool]
itIsMystery xs =
  map (\x -> elem x "aeiou") xs

-- this function takes a String and returns a list
-- stating whether or not each character is a vowel

-- 6)

reverseThree :: (Eq a, Num a) => [a] -> [a]
reverseThree xs =
  map findThree xs
  where findThree x = bool x (-3) (x == 3)
