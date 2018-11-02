-- ch16/ex10.hs

module Exercise where

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- 2)
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x
