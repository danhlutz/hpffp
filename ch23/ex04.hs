-- ch23/ex04.hs

module MyState where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s
                               in (f a, b)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) =
    Moi $ \s -> let (fab, s') = f s
                    (a, s'') = g s'
                in (fab a, s'')

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                  (Moi sb) = g a
                              in sb s'

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
