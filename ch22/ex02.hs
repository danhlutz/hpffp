-- ch22/ex02.hs

module Practice where

newtype Reader r a =
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

ask :: Reader a a
ask = Reader id
