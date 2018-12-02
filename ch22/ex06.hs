-- ch22/ex06.hs

module Exercise where

import Control.Applicative
import Data.Maybe

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' m ((i, v):ns) =
  if m == i
  then Just v
  else lookup' m ns

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) (z' n)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = do
  x' <- fst
  y' <- snd
  return $ f x' y'

summed :: Num c => (c, c) -> c
summed = do
  x' <- fst
  y' <- snd
  return $ (+) x' y'

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

bolt :: Integer -> Bool
bolt = (&&) <$>  (>3) <*> (<8)

bolt' :: Integer -> Bool
bolt' = do
  x' <- (>3)
  y' <- (<8)
  return $ x' && y'

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  putStrLn "New stuff here"
  -- 1
  print $ foldr (&&) True (sequA 14)
  -- 2
  print $ sequA (fromMaybe 0 s')
  -- 3
  print $ bolt (fromMaybe 0 ys) 
