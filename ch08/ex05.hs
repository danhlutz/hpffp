-- ex05.hs
module MyDivision where

data DividedResult = 
    Result Integer
  | DividedByZero
  deriving Show

resultAmount :: DividedResult -> Integer
resultAmount (Result amount) = amount

myDiv :: Integral a => a -> a -> DividedResult
myDiv num denom = go num denom 0
  where go n  d count
         | d == 0    = DividedByZero
         | d < 0     = go (negate n) (negate d) count
         | n < 0     = 
           Result (negate (resultAmount (go (negate n) d count)))
         | n < d     = Result count
         | otherwise =
             go (n - d) d (count + 1)
