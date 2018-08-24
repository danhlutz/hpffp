-- ex05.hs
module Comprehension where

mySqr :: Integral a => [a]
mySqr = [ x ^ 2 | x <- [1..10] ]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

a :: Integral a => [a]
a = [x | x <- mySqr, rem x 2 == 0]

a_guess = [4, 16, 36, 64, 100]

b :: Integral a => [(a, a)]
b = [(x, y) | x <- mySqr, 
              y <- mySqr,
              x < 50, y > 50]

b_guess = [(1, 64),  (1, 81),  (1, 100),
           (4, 64),  (4, 81),  (4, 100),
           (9, 64),  (9, 81),  (9, 100),
           (16, 64), (16, 81), (16, 100),
           (25, 64), (25, 81), (25, 100),
           (36, 64), (36, 81), (36, 100),
           (49, 64), (49, 81), (49, 100)]

c :: [(Integer, Integer)]
c = take 5 [ (x, y) | x <-mySqr,
                      y <-mySqr, 
                      x < 50, y > 50]

c_guess = [(1, 64), (1, 81), (1, 100),
           (4, 64), (4, 81)]

main :: IO ()
main = 
  do
      print $ "a equal? --> " ++ show (a == a_guess)
      print $ "b equal? --> " ++ show (b == b_guess)
      print $ "c equal? --> " ++ show (c == c_guess)
