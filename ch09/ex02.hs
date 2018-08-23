-- ex02.hs
module Symmetry where

myWords :: String -> [String]
myWords x
    | x == ""   = []
    | otherwise = 
       (takeWhile (/=' ') x) : (myWords (go x))
    where go n = 
            dropWhile (==' ') (dropWhile (/=' ') n)
