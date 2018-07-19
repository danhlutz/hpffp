--fun.hs
module Fib where

fib :: Int -> Int
fib n = helper 1 0 n

helper :: Int -> Int -> Int -> Int
helper b a 0 = a
helper b a n = helper (b + a) b (n - 1)
