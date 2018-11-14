-- ch18-03.hs

module Practice where

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceAgain :: [Integer] -> [Integer]
twiceAgain xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []
