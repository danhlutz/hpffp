-- ch10-01.hs
module MyFoldr where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs) 

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs =
  foldr (\x b -> f x || b) False xs
