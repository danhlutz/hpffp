-- ch16/ex04.hs
-- fmap exercises

module Exercises where

-- 1)
a = fmap (+1) $ read "[1]" :: [Int]

-- 2)
b = (fmap . fmap) (++ " lol") (Just ["Hi,", "Hello"])

-- 3)
c = fmap (*2) (\x -> x - 2)

-- 4)
d =
  fmap ((return '1' ++) . show)
       (\x -> [x, 1..3])
