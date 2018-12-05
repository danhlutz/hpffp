-- ch23-03.hs

module Practice where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5  == 0 = "Buzz"
  | mod n 3  == 0 = "Fizz"
  | otherwise     = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzList [1..100]
