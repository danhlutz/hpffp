-- ex01.hs
module MyEnums where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
   | x == y    = [x]
   | x > y     = []
   | otherwise = x : (eftBool (succ x) y)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
   | x == y    = [x]
   | x > y     = []
   | otherwise = x : (eftOrd (succ x) y)

eftInt :: Integer -> Integer -> [Integer]
eftInt x y
   | x == y    = [x]
   | x > y     = []
   | otherwise = x : (eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar x y
   | x == y    = [x]
   | x > y     = []
   | otherwise = x : (eftChar (succ x) y)
