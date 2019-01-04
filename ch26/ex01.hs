-- ch26/ex01.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

import Control.Monad (join)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- 1
instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT ema) =
    EitherT $ (fmap . fmap) f ema

-- 2
instance Applicative m
      => Applicative (EitherT e m) where
  pure x = EitherT $ (pure (pure x))

  (EitherT fema) <*> (EitherT ema) =
    EitherT $ (<*>) <$> fema <*> ema

-- 3
instance Monad m
      => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT ema) >>= f =
    EitherT $ do
      -- ema :: m (Either e a)
      -- v :: Either e a
      v <- ema
      case v of
        (Left x) -> return (Left x)
        (Right y) -> runEitherT (f y)

-- 4

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right y) = Left y

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- 5
eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) =
  -- join $ fmap (either f g) amb
  do
    v <- amb
    either f g v
