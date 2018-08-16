-- ch07-08.hs
module ChapterPractice where

myPrint :: Show a => a -> IO ()
myPrint = putStrLn . show
