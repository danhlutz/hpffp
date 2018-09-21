-- ex12.hs
module PracticeAsPatterns where

import Data.Char

-- example
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- 1)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf sub@(x:xs) sup@(m:ms)
  | xs  == []  = x == m || (isSubseqOf sub ms)
  | x   == m   = isSubseqOf xs ms
  | otherwise  = isSubseqOf sub ms

-- 2) capitalize
capitalizeWords :: String -> [(String, String)]
capitalizeWords x =
  map capit (words x)
  where capit w@(x:xs) = (w, (toUpper x) : xs)
