--fun.hs
module Fib where

fib :: Integer -> Integer
fib n = helper 1 0 n

helper :: Integer -> Integer -> Integer -> Integer
helper b a 0 = a
helper b a n = helper (b + a) b (n - 1)
