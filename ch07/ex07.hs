-- ex07.hs
module Exercise07 where

-- 1)
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

-- a)
tensDigitA :: Integral a => a -> a
tensDigitA x = d
    where xLast = fst (divMod x 10)
          d     = snd (divMod xLast 10)

-- b) yes they have the same type

-- c)
hunsD :: Integral a => a -> a
hunsD x = h
    where xLast = div x 100
          h     = mod xLast 10

-- 2)
foldBool :: a -> a -> Bool -> a
foldBool x y truth =
  case truth of
    False -> x
    True  -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y truth
    | truth == False = x
    | otherwise      = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
