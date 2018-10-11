module Numwords where

numToWord :: Integer -> String
numToWord x
  | x <  0 = "negative " ++ (numToWord (negate x))
  | x == 0 = "zero"
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"
  | otherwise = (numToWord front) ++ " " ++ (numToWord back)
  where front = quot x 10
        back  = rem  x 10

wordToNum :: String -> Integer
wordToNum x = helper (words x)

helper x =
  case x of
      ("negative":xs) -> negate (helper xs)
      otherwise -> foldl (\ a b -> (a * 10) + b) 0
                 (map wordDigit x)

wordDigit :: String -> Integer
wordDigit x
  | x == "zero" = 0
  | x == "one"  = 1
  | x == "two"  = 2
  | x == "three" = 3
  | x == "four" = 4
  | x == "five" = 5
  | x == "six"  = 6
  | x == "seven" = 7
  | x == "eight" = 8
  | x == "nine" = 9
