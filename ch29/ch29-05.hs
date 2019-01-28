-- ch29-05.hs

module Practice where

import Control.Concurrent

main :: IO ()
main = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero
