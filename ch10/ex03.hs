-- ex03.hs
module ScanEx where

-- 1)
fibs = take 20 $ 0 : scanl (+) 1 fibs

-- 2)
fibsL = takeWhile (<100) $ 0 : scanl (+) 1 fibsL

-- 3)
factorial n = factorials !! n
  where factorials = scanl (*) 1 [1..]
