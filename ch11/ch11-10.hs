-- ch11-10.hs
module Practice11 where

data Silly a b c d =
  MkSilly a b c d deriving Show

data MyList a = Nil | Cons a (MyList a) deriving Show
