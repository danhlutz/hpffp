-- ex05.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MoreTooManys where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany FunPair where
  tooMany (FunPair (n, _)) = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show, TooMany)

newtype FunPair = FunPair (Int, String) deriving (Eq, Show)
