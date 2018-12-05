-- ch23/ex04.hs

module MyState where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s)}

-- 1

get :: Moi s s
get = Moi (\x -> (x, x))

-- 2

put :: s -> Moi s ()
put s = Moi (\ _ -> ((), s))

-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

-- 5
modify :: (s -> s) -> Moi s ()
modify f = Moi (\x -> ((), f x))
