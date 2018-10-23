-- ch15/ex04.hs
module Practice where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- Optional type
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (mappend x y)

optGen :: (Arbitrary a) => Gen (Optional a)
optGen = do
  a <- arbitrary
  frequency [ (1, return Nada)
            , (3, return $ Only a )]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optGen

-- new work for Ex 04
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' (Only x)
  mappend _ x = x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  x <- arbitrary
  return $ First' x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = 
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool
                  

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
