-- ch17/ex09.hs

module Exercise where

import Control.Applicative (liftA3)

main :: IO ()
main = do
  putStrLn "combining ... "
  putStrLn $ show $ combos' stops vowels stops

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b , c)]
combos x y z = (,,) <$> x <*> y <*> z

combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' x y z = liftA3 (,,) x y z
