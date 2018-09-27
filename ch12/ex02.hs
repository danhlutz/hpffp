-- ex02.hs
module Exercises where

import Data.List (intersperse)

-- 1)
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe str = concat $ intersperse " " (helper (words str))

helper :: [String] -> [String]
helper [] = []
helper (w:ws)
  | notThe w == Nothing = "a" : helper ws
  | otherwise           = w : helper ws

-- 2)
vowelInitiated :: String -> Bool
vowelInitiated (x:xs) = elem x "aeiou"

countHelp :: [String] -> Integer
countHelp [] = 0
countHelp (x:[]) = 0
countHelp (a:b:bs)
  | a == "the" && (vowelInitiated b) = 1 + countHelp (b:bs)
  | otherwise = countHelp (b:bs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = countHelp (words str)

-- 3)
countVowels :: String -> Integer
countVowels [] = 0
countVowels (w:ws)
  | isVowel w = 1 + countVowels ws
  | otherwise = countVowels ws

isVowel :: Char -> Bool
isVowel x = elem x "aAeEiIoOuU"

-- 3)
vowel :: Char -> Maybe Char
vowel x
  | elem x "aeiouAEIOU" = Just x
  | otherwise = Nothing

countVowels' :: String -> Integer
countVowels' "" = 0
countVowels' (x:xs)
  | vowel x == Nothing = countVowels' xs
  | otherwise          = 1 + countVowels' xs
