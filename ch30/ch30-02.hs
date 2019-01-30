-- ch30-02a.hs

module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Running main cause an error!\
            \ It was: "
           ++ show e)
  writeFile "bbb" "hi"

main :: IO ()
main = do
  writeFile "zzz" "hi"
    `catch` handler
