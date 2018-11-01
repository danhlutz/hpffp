-- ch16/ch16-12.hs

{-# LANGUAGE FlexibleInstances #-}

module Practice where

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple e) where
  fmap f (Flip (Tuple a b)) =
    Flip $ Tuple (f a) b

