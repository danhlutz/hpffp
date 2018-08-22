-- ex07.hs
module WordNumer where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
   | n == 0    = "zero"
   | n == 1    = "one"
   | n == 2    = "two"
   | n == 3    = "three"
   | n == 4    = "four"
   | n == 5    = "five"
   | n == 6    = "six"
   | n == 7    = "seven"
   | n == 8    = "eight"
   | otherwise = "nine"

digits :: Int -> [Int]
digits n = reverse (go (abs n))
  where go x
         | x < 10 = [x]
         | otherwise = (mod x 10):(go (div x 10))

wordNumber :: Int -> String
wordNumber n = 
  concat 
    (intersperse "-" 
                 (map digitToWord (digits n)))
