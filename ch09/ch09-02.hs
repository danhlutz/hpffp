-- ch09-02.hs
module ChapterPractice where

acro :: String -> String
acro xs = [ x | x <- xs, elem x ['A'..'Z']]

vowels :: String -> String
vowels xs = [ x | x <- xs, elem x "aeiou" ]
