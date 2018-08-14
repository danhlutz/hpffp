-- ex02.hs
module Ex02 where

-- 1) 

-- a)
k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4-1), 10)

-- b)
k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

-- c) k1 and k3 both return 3

-- 2)

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (x, _, y) (x', _, y') = ((x, x'), (y, y'))
