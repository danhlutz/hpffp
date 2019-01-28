-- ch29-04.hs

module Practice where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) =
        toGregorian (utctDay t)
  case even dayOfMonth of
    False -> return $ Left randomIO
    True ->
      return $
      Right (putStrLn "no soup for you")
