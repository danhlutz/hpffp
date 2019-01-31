-- ch30-06.hs

module Practice where

import Control.Exception

data NotDivThree =
  NotDivThree Int
  deriving (Eq, Show)

instance Exception NotDivThree

data NotEven =
  NotEven Int
  deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i     = throwIO (NotEven i)
  | otherwise = return i

catchNotDivThree :: IO Int
                 -> (NotDivThree -> IO Int)
                 -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int
             -> (NotEven -> IO Int)
             -> IO Int
catchNotEven = catch

catchBoth ioInt =
  catches ioInt
  [ Handler
      (\(NotEven _) -> return maxBound)
  , Handler
      (\(NotDivThree _) -> return minBound)
  ]
