-- practice/tests/tests.hs
module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  putStrLn "testing monoid associativity for String"
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  putStrLn ""
  putStrLn "testing left identity of String"
  quickCheck (monoidLeftIdentity :: String -> Bool)
  putStrLn ""
  putStrLn "testing right identity of String"
  quickCheck (monoidRightIdentity :: String -> Bool)
  putStrLn ""
  putStrLn "Testing my new data type Bull"
  putStrLn "testing associativity"
  quickCheck (ma :: BullMappend)
  putStrLn "Testing left identity"
  quickCheck (mli :: Bull -> Bool)
  putStrLn "Testing right identity"
  quickCheck (mlr :: Bull -> Bool)

-- Associativity
monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- Identity
monoidLeftIdentity :: (Eq m, Monoid m)
                   => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- newDataType
data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = 
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend Fools x = x
  mappend x Fools = x
  mappend _ _     = Twoo

type BullMappend = Bull -> Bull -> Bull -> Bool
