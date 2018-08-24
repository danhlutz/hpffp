-- ex06.hs
module SquareCube where

mySqr :: Integral a => [a]
mySqr  = [ x ^ 2 | x <- [1..5] ]
-- [1, 4, 9, 16, 25]

myCube :: Integral a => [a]
myCube = [ x ^ 3 | x <- [1..5] ]
-- [1, 8, 27, 64, 125]

-- 1)
myTup :: Integral a => [(a, a)]
myTup = [ (x, y) | x <- mySqr, y <- myCube ]

-- 2)
fifty :: Integral a => a
fifty = 50

myTup2 :: Integral a => [(a, a)]
myTup2 = [ (x, y) | x <- mySqr,
                    y <- myCube,
                    x < fifty, 
                    y < fifty ]

-- 3)
numTuples :: Int
numTuples = length myTup2
