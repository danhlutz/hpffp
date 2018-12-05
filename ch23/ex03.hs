-- ch23-03.hs

module Practice where

fizzBuzz :: Integer -> String
fizzBuzz n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5  == 0 = "Buzz"
  | mod n 3  == 0 = "Fizz"
  | otherwise     = show n

fizzBuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzBuzzFromTo a b
  | a <= b    = (fizzBuzz a) : (fizzBuzzFromTo (a + 1) b)
  | otherwise = []

main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzFromTo 1 100
