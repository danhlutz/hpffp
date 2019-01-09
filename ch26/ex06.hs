-- ch26/ex06.hs

module Exercise where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad (liftM)

-- 1

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT x) = MaybeT $ (fmap . fmap) f x

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT mf) <*> (MaybeT ma) =
    MaybeT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= f =
    MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      (Just z) -> runMaybeT (f z)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (MonadIO m)
      => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> (m a) }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \_ -> pure x

  (ReaderT mf) <*> (ReaderT ma) =
    ReaderT $ \r -> (mf r) <*> (ma r)

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- (rma r)
      runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- 3

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

firstHelp :: (a -> b) -> (a, c) -> (b, c)
firstHelp f (x, y) = (f x, y)

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ (fmap . fmap) (firstHelp f) sma

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (StateT fsma) <*> (StateT sma) =
    StateT $ \s1 -> do
      (a, s2) <- sma s1
      (f, s3) <- fsma s2
      return (f a, s3)

instance Monad m => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s1 -> do
      (a, s2) <- sma s1
      runStateT (f a) s2

instance MonadTrans (StateT s) where
  lift x = StateT $ \s -> do
    a <- x
    return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
