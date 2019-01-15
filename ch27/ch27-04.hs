-- ch27-04.hs

module Practice where

strictTest :: Bool -> Int
strictTest b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True  -> 1
