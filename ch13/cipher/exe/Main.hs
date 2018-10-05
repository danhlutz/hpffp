module Main where

import Cipher

main :: IO ()
main = do
  putStrLn "VIGENERE CIPHER BUDDY"
  putStrLn "---------------------"
  encoder <- pickEncoder
  msg     <- getMessage
  secret  <- getSecret
  putStr $ "ENCODING MESSAGE ... " ++ msg
  putStrLn $ " WITH ... " ++ secret
  putStrLn "---------------------"
  putStrLn $ encoder msg secret

getMessage :: IO String
getMessage = do
  putStrLn "WHAT IS YOUR MESSAGE?"
  msg <- getLine
  return msg

getSecret :: IO String
getSecret = do
  putStrLn "AND WHAT IS YOUR SECRET?"
  secret <- getLine
  return secret

pickEncoder :: IO (String -> String -> String)
pickEncoder = do
  putStrLn "CHOOSE MODE: 'encode' OR 'decode'"
  choice <- getLine
  if choice == "decode"
  then do
    putStrLn "ENTERING DECODE MODE"
    (return decodeVig)
  else do
    putStrLn "ENTERING ENCODE MODE"
    (return encodeVig)
