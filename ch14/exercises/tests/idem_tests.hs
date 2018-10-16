-- idem_tests.hs
module IdemTests where

import Exercises
import Data.Char (toUpper)
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Testing idempotence"
  putStrLn "capitalizeWord multiple times does not change the value"
  putStrLn "after the first application"
  quickCheck prop_capIdemPotent
  putStrLn "After sorting is applied once, subsequent applications"
  putStrLn "do not change the returned value"
  quickCheck prop_sortIdemPotent
  putStrLn "Every element in a sorted list is <= to the next"
  quickCheck prop_listSorted

twice f = f . f
fourTimes = twice . twice

-- 1)
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (c:cs) = (toUpper c) : cs

prop_capIdemPotent :: String -> Bool
prop_capIdemPotent s =
     (capitalizeWord s == twice capitalizeWord s)
  && (capitalizeWord s == fourTimes capitalizeWord s)

-- 2)
mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort (x:[]) = [x]
mySort (x:xs) =
    (mySort (fst bunched))
 ++ [x]
 ++ (mySort (snd bunched))
 where bunched = bunch x xs

bunch :: Ord a => a -> [a] -> ([a], [a])
bunch x ys = go x ys [] []
  where go comp [] lt gte = (lt, gte)
        go comp (a:as) lt gte
         | a < comp  = go comp as (a:lt) gte
         | otherwise = go comp as lt (a:gte)

inOrder :: Ord a => [a] -> Bool
inOrder [] = True
inOrder (_:[]) = True
inOrder (x:y:ys) = x <= y && inOrder (y:ys)

prop_sortIdemPotent :: [Integer] -> Bool
prop_sortIdemPotent xs =
     (mySort xs == twice mySort xs)
  && (mySort xs == fourTimes mySort xs)

prop_listSorted :: String -> Bool
prop_listSorted = inOrder . mySort
