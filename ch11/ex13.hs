-- ex13.hs
module Exercises11 where

import Data.Char

-- 1)
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) = (toUpper x) : xs

-- 2)
capitalizeParagraph :: String -> String
capitalizeParagraph xs = go xs "" True
  where go "" result start = (reverse result)
        go (w:ws) result start
           | w == ' '  = go ws (w:result) start
           | w == '.'  = go ws (w:result) True
           | start     = go ws (toUpper w : result) False
           | otherwise = go ws (w:result) start
  
