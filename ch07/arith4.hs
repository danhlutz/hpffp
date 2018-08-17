-- arirth4.hs
module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTripPF' :: (Show a, Read b) => a -> b
roundTripPF' = read . show

main :: IO ()
main = do
  print ((roundTripPF' 4) :: Double)
  print (id 4)
