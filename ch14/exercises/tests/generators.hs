-- tests/generators.hs
module RandomGenerators where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = do
  oneof [return Fulse,
         return Frue]

genWeightedFool :: Gen Fool
genWeightedFool = do
  frequency [(2, return Fulse),
             (1, return Frue)]
