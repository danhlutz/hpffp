-- ch30-02a.hs

module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn $ "We errored! It was" ++ show e

main :: IO ()
main = do
  writeFile "aaa" "hi"
    `catch` handler
