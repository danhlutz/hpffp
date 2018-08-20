-- ch08-03.hs
module Fib where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
  fibonacci (x - 1) + fibonacci (x - 2)

-- now this is how you do it!
fib :: Integral a => a -> a
fib n = fibHelp 1 0 n


fibHelp :: Integral a => a -> a -> a -> a
fibHelp b a 0 = a
fibHelp b a n = fibHelp (b + a) b (n - 1)
