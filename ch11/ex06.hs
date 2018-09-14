-- ex05.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module MoreTooManys where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- 1)
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2 & 3)
instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (m, n) = m + n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show, TooMany)
