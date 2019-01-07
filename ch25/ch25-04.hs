-- ch25/ex03.hs

{-# LANGUAGE InstanceSigs #-}

module Practice where

import Control.Monad (join)

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Functor m)
      => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT $ fmap f fa

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity x) = Identity (f x)

instance (Applicative m)
      => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT $ fab <*> fa

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance (Monad m)
      => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f

-- x >>= f
-- EQUALS
-- join (fmap f x)