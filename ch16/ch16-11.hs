-- ch16/ch16-11.hs

{-# LANGUAGE RankNTypes #-}

module Practice where

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- this will not work
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]



