-- ch26-02.hs

{-# LANGUAGE InstanceSigs #-}

module Practice where

newtype ReaderT r m a =
  ReaderT { rundReaderT :: r -> m a }

instance (Functor m)
      => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m)
      => Applicative (ReaderT r m) where
  pure x = ReaderT (pure (pure x))

  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m)
      => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      rundReaderT (f a) r
