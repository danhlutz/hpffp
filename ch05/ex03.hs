--ex03.hs
--Parametricity exercises
module Parametricity where

--1. This is the only way to write a function with this type
myId :: a -> a
myId x = x

--2. here are the two different versions of function
--   with the type a -> a -> a

aaaOne :: a -> a -> a
aaaOne x y = x

aaaTwo :: a -> a -> a
aaaTwo x y = y

--3. there is only one way to write a function 
--   with the type a -> b -> b
abb :: a -> b -> b
abb x y = y
