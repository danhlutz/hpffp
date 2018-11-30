-- ch22/ex04.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

newtype Reader r a =
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> id a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \ r -> rab r (ra r)
