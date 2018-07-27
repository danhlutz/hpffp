--ex10.hs
module FuncWriter where

-- 1)
i :: a -> a
i x = x

-- 2)
c :: a -> b -> a
c x y = x

-- 3)
c'' :: b -> a -> b
c'' x y = x

-- c'' and c are the same function

-- 4) 
c' :: a -> b -> b
c' x y = y

-- 5)
r :: [a] -> [a]
r x = x

r2 :: [a] -> [a]
r2 x = reverse x

-- 6)
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC (aToB x)

-- 7)
a :: (a -> c) -> a -> a
a aToC x = x

-- 8) 
a' :: (a -> b) -> a -> b
a' aToB x = aToB x
