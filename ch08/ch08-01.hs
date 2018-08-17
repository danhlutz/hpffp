-- ch08-01.hs
module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
three' = (inc . inc . inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n =
  1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => (b -> b) -> a -> b -> b
applyTimes f 0 b = b
applyTimes f n b = f (applyTimes f (n - 1) b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' = applyTimes (+1)
