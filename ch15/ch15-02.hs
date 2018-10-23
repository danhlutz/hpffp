-- ch15/ch15-02.hs
module Practice where

import Data.Monoid
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)
