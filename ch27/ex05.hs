-- ch27/ex05.hs

{-# LANGUAGE BangPatterns #-}

module Ex where

x = undefined

y = "blah"

wontBottom :: IO ()
wontBottom = do
  print (snd (x, y))

willBottom :: String
willBottom =
  seq x (snd (x,y))

willBottom' !n =
  print (snd (n, y))

willBottom'' = willBottom' x
