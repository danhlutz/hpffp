-- ch22/ex03.hs

module Exercise where

newtype Reader r a =
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

-- 1

myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f x y =
  f <$> x <*> y

-- 2
asks :: (r -> a) -> Reader r a
asks f = Reader f
