-- ch25/ex01.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = fmap Compose $ pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ ((<*>) <$> f) <*> a

-- NOT POSSIBLE
instance (Monad f, Monad g) =>
         Monad (Compose f g) where
  return = pure

  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (>>=) = _
