-- ex14.hs
module Phoney where

import Data.Char

type Digit = Char

type Presses = Int

data DaPhone = DaPhone [(Digit, String)]

myPhone :: DaPhone
myPhone = 
  DaPhone [('1', "1"),     ('2', "abc2"), ('3', "def3"),
           ('4', "ghi4"),  ('5', "jkl5"), ('6', "mno6"),
           ('7', "pqrs7"), ('8', "tuv8"), ('9', "wxyz9"),
                           ('0', " 0"),   ('#', ".,#")  ]

convo :: [String]
convo = 
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol rofl" ,
   "Lol ya",
   "Wow ur coool haha. Ur turn.",
   "Ok. Do you think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn. rofl rofl rofl rofl"]

-- 2)
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p@(DaPhone ((digit, values) : rest)) char
 | isCapital char   = 
     ('*', 1) : reverseTaps p (toLower char)
 | elem char values = [(digit, (numPresses char values))] 
 | otherwise        = reverseTaps (DaPhone rest) char

numPresses :: Char -> String -> Int
numPresses c (x:xs)
  | c == x    = 1
  | otherwise = 1 + (numPresses c xs)

isCapital :: Char -> Bool
isCapital x = elem x ['A'..'Z']

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone msg = 
  concat (map (\ x -> reverseTaps phone x) msg)

-- 3)
getPresses :: (Digit, Presses) -> Presses
getPresses (_, p) = p

getListPresses :: [(Digit, Presses)] -> Presses
getListPresses xs = foldr (+) 0 (map getPresses xs)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr (+) 0 (map getPresses xs)

-- 4)
mostPopular:: Eq a => [a] -> a
mostPopular f@(x:xs) = go x (occurences x f) xs
  where go tmpMax _ [] = tmpMax
        go tmpMax count (y:ys)
           | count < (occurences y f) = go y (occurences y f) ys
           | otherwise = go tmpMax count ys

mostPopularLetter :: String -> Char
mostPopularLetter msg@(x:xs) = go x (occurences x msg) xs
  where go tmpMax _ [] = tmpMax
        go tmpMax count (y:ys)
           | count < (occurences y msg) = go y (occurences y msg) ys
           | otherwise = go tmpMax count ys

mostPopularLetter' :: String -> Char
mostPopularLetter' msg = mostPopular msg

occurences :: Eq a => a -> [a] -> Int
occurences x [] = 0
occurences x (y:ys)
  | x == y    = 1 + occurences x ys
  | otherwise = occurences x ys

letterCost :: Char -> String -> Int
letterCost c xs = foldr (+) 0 presses
  where presses = map (\ x -> if c == x
                              then getListPresses (reverseTaps myPhone c)
                              else 0)
                      xs
 
mostPopularCost :: [String] -> [Int]
mostPopularCost [] = []
mostPopularCost (x:xs) =
  letterCost (mostPopularLetter x) x : mostPopularCost xs

-- 5) 
coolestLtr :: [String] -> Char
coolestLtr msgs = mostPopularLetter (concat msgs)

coolestWord :: [String] -> String
coolestWord msgs = go first count ms
  where ms = words (concat msgs)
        first = head ms
        count = occurences first ms
        go tmpMax count [] = tmpMax
        go tmpMax count (y:ys)
           | count < (occurences y ms) = go y (occurences y ms) ys
           | otherwise = go tmpMax count ys

coolestWord' :: [String] -> String
coolestWord' msgs = mostPopular (words (concat msgs))
