-- ch30-04.hs

module Practice where

import Control.Exception

canICatch :: Exception e
          => e -> IO (Either ArithException ())
canICatch e =
  try $ throwIO e
