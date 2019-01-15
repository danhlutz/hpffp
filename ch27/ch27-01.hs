-- ch27-01.hs

module Practice where

possiblyKaboom =
  \f -> f fst snd (0, undefined)

possiblyKaboom' b =
  case b of
    True  -> fst tup
    False -> snd tup
 where tup = (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

foldr' k z xs = go xs
  where
    go [] = z
    go (y:ys) = k y (go ys)

c = foldr' const undefined ['a', undefined]
