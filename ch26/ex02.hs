-- ch26/ex02.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

-- 1

fstTupF :: (a -> b) -> (a, c) -> (b, c)
fstTupF f (x, y) = (f x, y)

instance (Functor m)
      => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ (fmap . fmap) (fstTupF f) sma

-- instance (Applicative m)
--       => Applicative (StateT s m) where
--  pure x = StateT $ \s -> pure (x, s)
--
--  (StateT fsma) <*> (StateT sma) =
--    StateT $ \s ->
--      let mf = fst <$> (fsma s)
--          mas = sma s
--      in fstTupF <$> mf <*> mas

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
