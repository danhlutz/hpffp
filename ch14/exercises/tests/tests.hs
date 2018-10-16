-- tests/tests.hs

module Main where

import Data.List (sort)
import Exercises
import Test.QuickCheck

-- 1)
main :: IO ()
main = quickCheck prop_halfIdentity

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = (2 * (half x)) == x

-- 2) test sort

testSort :: IO ()
testSort = quickCheck prop_sorted

prop_sorted :: [Integer] -> Bool
prop_sorted xs = listOrdered (sort xs)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

-- 3) Associativity and Commutatity
plusAssociative :: Integral a => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Integral a => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

prop_associative :: Integer -> Integer -> Integer -> Bool
prop_associative = plusAssociative

test_associative :: IO ()
test_associative = quickCheck prop_associative

prop_commutative :: Integer -> Integer -> Bool
prop_commutative = plusCommutative

test_commutative :: IO ()
test_commutative = quickCheck prop_commutative

-- 4) Multiplication associativity and commutativity
multAssociative :: Integer -> Integer -> Integer -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: Integer -> Integer -> Bool
multCommutative x y = x * y == y * x

qcMult = do
  quickCheck multAssociative
  quickCheck multCommutative

-- 5)
quotCombines :: Integer -> Integer -> Bool
quotCombines x y =
     y == 0
  || (quot x y) * y + (rem x y) == x

divCombines :: Integer -> Integer -> Bool
divCombines x y =
     y == 0
  || (div x y) * y + (mod x y) == x

qcDiv :: IO ()
qcDiv = do
  quickCheck quotCombines
  quickCheck divCombines

-- 6)
powerAssociative :: Integer -> Integer -> Integer -> Bool
powerAssociative x y z =
 x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Integer -> Integer -> Bool
powerCommutative x y = x ^ y == y ^ x

qcPower :: IO ()
qcPower = do
  putStrLn "Testing associativity"
  quickCheck powerAssociative
  putStrLn "Testing commutativity"
  quickCheck powerCommutative

-- 7)
doubleReverse :: String -> String
doubleReverse = reverse . reverse

prop_reverseId :: String -> Bool
prop_reverseId s = (doubleReverse s) == id s

qcReverse :: IO ()
qcReverse = do
  putStrLn "testing reverse"
  quickCheck prop_reverseId

-- 8) I'm having trouble figuring out how to generate the appropriate f's

-- 9)
prop_concatCons :: String -> String -> Bool
prop_concatCons s t =
  foldr (:) t s == s ++ t

prop_concatCons2 :: String -> Bool
prop_concatCons2 s =
  foldr (:) [] s == concat [s, []]

qcStrings :: IO ()
qcStrings = do
  putStrLn "check foldr and Cons"
  quickCheck prop_concatCons
  putStrLn "Checking with base case defined"
  quickCheck prop_concatCons2

-- 10)
prop_lenTake :: Int -> String -> Bool
prop_lenTake n xs =
  length (take n xs) == n

qcLenTake :: IO ()
qcLenTake = do
  putStrLn "The length of taking n items from a list equals n"
  quickCheck prop_lenTake
  putStrLn "This is not true in the case when the list is shorter than n"

-- 11)
prop_circleBack :: String -> Bool
prop_circleBack x = (read (show x)) == x

qcReadShow :: IO ()
qcReadShow = do
  putStrLn "reading a shown piece of data equals the original datum"
  quickCheck prop_circleBack

-- 12)
square x = x * x

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x =
  (square (sqrt x)) == x

qcSquareSqrt :: IO ()
qcSquareSqrt = do
  putStrLn "Does the square of the sqrt of a number equal the number?"
  quickCheck prop_squareIdentity
  putStrLn "No, because of the inaccuracies in floating point arithmetic."
