-- ch18/ex05.hs

module Exercise where

-- 1

j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = fmap f xs

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f xs ys = f <$> xs <*> ys

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a xs fs = fs <*> xs

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f)
