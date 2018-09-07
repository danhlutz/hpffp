-- ex02.hs
module DBProcess where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  , DbDate (UTCTime
             (fromGregorian 1917 5 1)
             (secondsToDiffTime 34999))
  , DbNumber 711
  ]


-- 1)

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate []                   = []
filterDbDate ((DbDate time) : xs) = time : filterDbDate xs
filterDbDate (_ : xs)             = filterDbDate xs

-- 2)
filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber a) : xs) = a : filterDbNumber xs
filterDbNumber (_ : xs)            = filterDbNumber xs

-- 3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr max first dates
  where dates = (filterDbDate xs)
        first = head dates

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb xs = sum / count
  where sum = fromIntegral (sumDb xs)
        count = fromIntegral (length $ filterDbNumber xs)
