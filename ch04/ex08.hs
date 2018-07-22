--ex08.scm
--syntax corrections
module Corrections where

-- 1.

x = (+)

f :: [a] -> Int
f xs = w `x` 1
  where w = length xs

-- 2.

ident :: a -> a
ident x = x

-- 3.

g :: (a, b) -> a
g (a, b) = a
