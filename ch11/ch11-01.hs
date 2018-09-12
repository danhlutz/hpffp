-- ch11-01.hs
module Dog where

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge =
  DogueDeBordeaux doge
  deriving Show
