-- ch18/ch18-01.hs

module Practice where

import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
  putStrLn "Blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah 2">>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah 3" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn
