-- ch26/ex08.hs
-- chapter exercise: Fix the code

module Ex where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad

isValid :: String -> Bool
isValid v = elem '!' v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift (getLine)
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something exciting!"
  excite <- runMaybeT (maybeExcite)
  case excite of
    Nothing -> putStrLn "MORE EXCITE!"
    Just e ->
      putStrLn ("Good was very excite: " ++ e)
