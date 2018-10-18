-- Cipher.hs
module Cipher 
       ( encodeVig, decodeVig )
       where

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

vigenere :: Char -> Char -> Char
vigenere a b = caesar a (ord b)

unVigenere :: Char -> Char -> Char
unVigenere a b = unCaesar a (ord b)

defaultKey :: String
defaultKey = "thisismyfunplace"

encodeVig :: String -> String -> String
encodeVig "" _ = ""
encodeVig x "" = encodeVig x defaultKey
encodeVig (x:xs) (w:ws)
  | x == ' '  = ' ' : (encodeVig xs (w:ws))
  | otherwise = 
        (vigenere x w)
      : (encodeVig xs (ws ++ [w]))

decodeVig :: String -> String -> String
decodeVig "" _ = ""
decodeVig x "" = decodeVig x defaultKey
decodeVig (x:xs) (w:ws)
  | x == ' ' = ' ' : (decodeVig xs (w:ws))
  | otherwise =
        (unVigenere x w)
      : (decodeVig xs (ws ++ [w]))
