-- ch18-07.hs

module Practice where

import Control.Monad (join)

mcomp :: Monad m
      => (b -> m c)
      -> (a -> m b)
      -> a -> m c
mcomp f g a = join $ f <$> (g a)

mcomp' :: Monad m
       => (b -> m c)
       -> (a -> m b)
       -> a -> m c
mcomp' f g a = g a >>= f
