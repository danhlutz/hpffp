-- cipher.hs
module Cipher where

import Data.Char

caesar :: Char -> Int -> Char
caesar c i = chr x
  where x = (mod ((ord c) - 97 + i) 26) + 97

unCaesar :: Char -> Int -> Char
unCaesar c i = chr x
  where x = (mod ((ord c) - 97 - i) 26) + 97

encode :: String -> Int -> String
encode s i = map (\x -> caesar x i) lowered
  where lowered = map toLower s

decode :: String -> Int -> String
decode s i = map (\x -> unCaesar x i) s
