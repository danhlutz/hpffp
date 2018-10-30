-- ch16/ch16-05.hs
{-# LANGUAGE ViewPatterns #-}

module Practice where

import Test.QuickCheck
import Test.QuickCheck.Function

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

functorCompose' :: (Eq (f c), Functor f)
                => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
