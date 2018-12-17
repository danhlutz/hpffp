-- ch24/ex05.hs

module Exercise where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Text.Trifecta

parseS :: Parser a -> String -> Result a
parseS p str = parseString p mempty str

parseDigit :: Parser Char
parseDigit = foldr (<|>) (char '9') (char <$> ['0'..'8'])

base10Integer' :: Parser Integer
base10Integer' =
      try negBase10
  <|> base10Integer

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return $ digStrToInt digits

negBase10 :: Parser Integer
negBase10 = do
  _ <- char '-'
  number <- base10Integer
  return $ negate number

digitValues :: M.Map Char Integer
digitValues =
  M.fromList $ zip ['0'..'9'] [0..9]

digToInt :: Char -> Integer
digToInt c =
  case (result c) of
    (Just x) -> x
    Nothing  -> 0
  where result y = M.lookup y digitValues

digStrToInt :: String -> Integer
digStrToInt xs = go 0 xs
  where go :: Integer -> String -> Integer
        go sofar "" = sofar
        go sofar (n:ns) = go (sofar * 10 + (digToInt n)) ns
