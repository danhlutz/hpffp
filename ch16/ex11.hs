-- ch16/ex11.hs

{-# LANGUAGE FlexibleInstances #-}

module Exercise where

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))
