-- ch15-01.hs
module Practice where

data Booly a =
    False'
  | True'
  deriving (Eq, Show)

instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'
