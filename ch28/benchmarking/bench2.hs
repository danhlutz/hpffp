-- ch28/bench2.hs

module Main where

import Criterion.Main
import Debug.Trace

infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n <0       = Nothing
  | otherwise =
    foldr
    (\x r k ->
       case k of
         0 -> Just x
         _ -> r (k-1))
    (const Nothing) xs n

myList :: [Int]
myList = trace "myList was evaluated" ([1..9999] ++ undefined)

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "Index list maybe index 9999"
    $ nf (myList !?) 9999]
