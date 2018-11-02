-- ch16/ex09.hs
-- make the Functor instance work

module Exercise where

-- 1)
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2)
data Company a b c =
    DeepBlue a c
  | Something b

instance Functor (Company a b) where
  fmap f (DeepBlue x z) = DeepBlue x (f z)
  fmap _ (Something y) = Something y

-- 3)
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L x y z) = L (f x) y (f z)
  fmap g (R m n o) = R m (g n) o
