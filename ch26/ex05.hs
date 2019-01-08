-- ch26/ex02.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

import Control.Monad (liftM)

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

fstTupF :: (a -> b) -> (a, c) -> (b, c)
fstTupF f (x, y) = (f x, y)

instance (Functor m)
      => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ (fmap . fmap) (fstTupF f) sma

instance (Monad m)
      => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x ,s)

  (StateT fsma) <*> (StateT sma) =
    StateT $ \s1 -> do
      (a, s2) <- sma s1
      (f, s3) <- fsma s2
      return (f a, s3)

instance (Monad m)
      => Monad (StateT s m) where
  return = pure

  (StateT smas) >>= f =
    StateT $ \s1 -> do
      (a, s2) <- smas s1
      runStateT (f a) s2


--

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

-- i couldn't do this part on my own. 
instance MonadTrans (StateT s) where
  lift x = StateT $ \s -> do
    a <- x
    return (a, s)
