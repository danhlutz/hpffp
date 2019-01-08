-- ch26/ex04.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

import Control.Monad (liftM)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT ema) =
    EitherT $ (fmap . fmap) f ema

instance Applicative m
      => Applicative (EitherT e m) where
  pure x = EitherT $ (pure (pure x))

  (EitherT fema) <*> (EitherT ema) =
    EitherT $ (<*>) <$> fema <*> ema

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


class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
