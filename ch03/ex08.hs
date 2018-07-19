--ex08.hs
module AwesomeCurry where

rvrs :: String
rvrs = let s = "Curry is awesome"
           in drop 9 s ++
              drop 5 (take 9 s) ++
              take 5 s
