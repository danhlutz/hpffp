--ex06.hs
module WhatCanWeDo where

data Rocks =
  Rocks String deriving (Eq, Ord, Show)

data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Ord, Show)

-- 1) phew = Papu "chases" True
-- will not type check. change to -> 
phew = Papu (Rocks "chases") (Yeah True)

-- 2) will typecheck!
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- 3) will typecheck
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4) will not typecheck unless we derive Ord for Rocks, Yeah, and Papu 
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
