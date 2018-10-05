-- ex02.hs
module Exercise where

import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True  -> do putStrLn "It's a palindrome!"
                exitSuccess
    False -> do putStrLn "Nope!"
                exitSuccess

letters :: String
letters = ['a'..'z'] ++ ['A'..'Z']

condense :: String -> String
condense "" = ""
condense (x:xs) =
 if elem x letters
 then (x : condense xs)
 else condense xs

lowerAll :: String -> String
lowerAll = map toLower

isPalindrome :: String -> Bool
isPalindrome phrase =
  flatten == (reverse flatten)
  where flatten = lowerAll (condense phrase)
