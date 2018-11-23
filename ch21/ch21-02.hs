-- ch21-02.hs

module Practice where

data MyEither a b =
    Lefty a
  | Righty b
  deriving (Eq, Ord, Show)

instance Functor (MyEither a) where
  fmap _ (Lefty x) = Lefty x
  fmap f (Righty y) = Righty (f y)

instance Applicative (MyEither a) where
  pure    = Righty
  Lefty e <*> _  = Lefty e
  Righty f <*> r = fmap f r

instance Foldable (MyEither a) where
  foldMap _ (Lefty _) = mempty
  foldMap f (Righty x) = f x

  foldr _ z (Lefty _) = z
  foldr f z (Righty y) = f y z

instance Traversable (MyEither a) where
  traverse _ (Lefty x)  = pure (Lefty x)
  traverse f (Righty y) = Righty <$> f y

--

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure x = Pair mempty x
  (Pair u f) <*> (Pair x y) = Pair (mappend u x) (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y
  foldr f z (Pair _ y) = f y z

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> (f y)
