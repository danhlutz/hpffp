-- ch23/ex02.hs

module MyState where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\x -> (f (fst (g x)), snd (g x)))


instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi (\x -> (a, x))

  -- (<*>) :: Moi s (a -> b)
  --       -> Moi s a
  --       -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \x -> ((fst (f x)) (fst (g x)) , snd (g x))

instance Monad (Moi s) where
  return = pure

  -- (>>=) :: Moi s a
  --       -> (a -> Moi s b)
  --       -> Moi s b
  (Moi f) >>= g = Moi $ \x -> (fst $ runMoi (g (fst (f x))) x, snd (f x))
