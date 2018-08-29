-- ex13.hs
module CharExercises where

import Data.Char

-- 1)
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2)
onlyUppers :: String -> String
onlyUppers = filter isUpper

-- 3)
proper :: String -> String
proper ""     = []
proper (x:xs) = toUpper x : xs

-- 4)
allCaps :: String -> String
allCaps ""     = []
allCaps (x:xs) = toUpper x : allCaps xs

-- 5)
properFirst :: String -> Char
properFirst "" = ' '
properFirst xs = (toUpper . head) xs

-- 6) point free
properFirstPF :: String -> Char
properFirstPF = toUpper . head
