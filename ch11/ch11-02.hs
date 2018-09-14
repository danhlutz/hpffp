-- ch11-02.hs
module Animals where

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
