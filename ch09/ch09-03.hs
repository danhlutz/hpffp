-- ch09-03.hs
module ChapterPractice where

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs
