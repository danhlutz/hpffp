{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineThistype where

example :: Num a => a
example = 1

-- 1)
a :: Num a => a
a = (* 9) 6

b :: Num a => (a, [Char])
b = head [(0, "doge"),(1, "kitteh")]

c :: (Integer, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

d :: Bool
d = if False then True else False

e :: Int
e = length [1, 2, 3, 4, 5]

f :: Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2)
x :: Num a => a
x = 5

y :: Num a => a
y = x + 5

w :: Num a => a
w = y * 10

-- 3)

z :: Num a => a -> a
z y = y * 10

-- 4) 

f4 :: Fractional a => a
f4 = 4 / y

-- 5)
x5 :: String
x5 = "Julie"

y5 :: String
y5 = "<3"

z5 :: String
z5 = "Haskell"

f5 :: String
f5 = x5 ++ y5 ++ z5
