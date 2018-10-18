-- module tests/tests.hs
module Main where

import Cipher
import Data.Char
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Vigenere cipher tests"
  putStrLn ""
  putStrLn "Testing encode message in range ['a'..'z'] and space"
  quickCheck prop_encodesToABCs
  putStrLn ""
  putStrLn "A message made up of the abcs can be decoded"
  quickCheck prop_reversible
  putStrLn ""
  putStrLn "encoding can take any character, but it only returns abcs"
  putStrLn "however, the order of the decoded encoded character"
  putStrLn "is congruent mod 26 with the original character"
  quickCheck prop_orderCongruence

-- tests

inABCs :: Char -> Bool
inABCs x = elem x $ ['a'..'z'] ++ " "

prop_encodesToABCs :: String -> String -> Bool
prop_encodesToABCs s k =
  and (map inABCs $ encodeVig s k)

-- NOTE: using QuickCheck and an Arbitrary String, you
-- must encode twice in order to reduce a very weird string into the
-- lowercase alphabet

prop_reversible :: String -> String -> Bool
prop_reversible msg k =
  decodeVig (encodeVig one_pass k) k == one_pass
  where one_pass = encodeVig msg k

prop_orderCongruence :: String -> String -> Bool
prop_orderCongruence msg k =
  and (zipWith congruent msg decoded)
  where decoded = decodeVig (encodeVig msg k) k

congruent :: Char -> Char -> Bool
congruent x y =
  mod (ord x) 26 == mod (ord y) 26
