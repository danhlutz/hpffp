-- ch16/ch16-04.hs
module Practice where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a))
                => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f)
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
