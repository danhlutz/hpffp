-- ch27/ex02.hs

{-# LANGUAGE Strict #-}

module Ex where

data List a =
    Nil
  | Cons a (List a)
  deriving (Show)

take' :: Int -> List a -> List a
take' n _  | n <= 0 = Nil
take' _ Nil         = Nil
take' n (Cons x ~xs) =
  Cons x (take' (n - 1) xs)

map' :: (a -> b) -> List a -> List b
map' _ Nil         = Nil
map' f (Cons x ~xs) = Cons (f x) (map' f xs)

repeat' :: a -> List a
repeat' x = xs where ~xs = Cons x xs

main :: IO ()
main = do
  print $ take' 10 $ map' (+1) (repeat' 1)

-- RESULTS
-- No Laziness
-- -> never terminates

-- Change data definition to Cons a ~(List a)
-- -> terminates

-- Change xs to ~xs in defintion of take'
-- -> never terminates

-- Change xs to ~xs in definition of take' repeat' and map'
-- -> never terminates
