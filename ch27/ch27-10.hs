-- ch27-10.hs

{-# LANGUAGE BangPatterns #-}

module Practice where

data DoesntForce =
  TisLazy Int String
  deriving (Show)

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

data BangBang =
  SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s
