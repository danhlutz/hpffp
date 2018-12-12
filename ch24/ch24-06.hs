-- ch24-06.hs

module Practice where

import Text.Trifecta

p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)
