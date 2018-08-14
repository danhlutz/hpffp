--ex01.hs
module GrabBag where

-- 1) which are equivalent
mTh1 x y z = x * y * z

mTh2 x y = \z -> x * y * z

mTh3 x = \y -> \z -> x * y * z

mTh4 = \x -> \y -> \z -> x * y * z

-- all four equivalent and follow Haskell's order of function application

-- 2) What is the type of mTh3?
mTh3 :: Num a => a -> a -> a -> a

-- 3) rewrite using anonymous lambda syntax

-- a)

addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f n = n + 1

addOneIfOdd2 =
  \n -> case odd n of
          True  -> f n
          False -> n
          where f n = n + 1

-- b)
addFive x y = (if x > y then y else x) + 5

addFive2 = \x -> \y -> (if x > y then y else x) + 5

-- c)
mflip f = \x -> \y -> f y x

mflip2 f x y = f y x
