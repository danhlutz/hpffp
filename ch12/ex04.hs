-- ex04.hs
module Exercise where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + (natToInteger nat)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just (intHelp x)

intHelp :: Integer -> Nat
intHelp 0 = Zero
intHelp x = Succ (intHelp (x - 1))
