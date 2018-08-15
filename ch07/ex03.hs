-- ex03.hs
module RewriteAsCase where

-- 1)
functionC x y = if (x > y) then x else y

functionC' :: Ord a => a -> a -> a
functionC' x y =
  case x > y of
    True  -> x
    False -> y

-- 2)
ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' :: Integral a => a -> a
ifEvenAdd2' n =
  case even n of
    True  -> n + 2
    False -> n

-- 3)
nums :: (Ord a, Num a) => a -> a
nums x =
  case compare x 0 of
    LT -> -1
    GT ->  1
    EQ ->  0
