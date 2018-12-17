-- ch26/ex06.hs

module Exercise where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Text.Trifecta

parseS :: Parser a -> String -> Result a
parseS p str = parseString p mempty str

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

toInt :: String -> Int
toInt str = go 0 str
  where go :: Int -> String -> Int
        go sofar "" = sofar
        go sofar (d:ds) = go (sofar * 10 + digitToInt d) ds

digitToInt :: Char -> Int
digitToInt c =
  case (M.lookup c digValues) of
    (Just x) -> x
    Nothing  -> 0
  where digValues = M.fromList $ zip ['0'..'9'] [0..9]

parseArea :: Parser NumberingPlanArea
parseArea = do
  a1 <- digit
  a2 <- digit
  a3 <- digit
  return $ toInt [a1, a2, a3]

parseExchange :: Parser Exchange
parseExchange = do
  e1 <- digit
  e2 <- digit
  e3 <- digit
  return $ toInt [e1, e2, e3]

parseLineNumber :: Parser LineNumber
parseLineNumber = do
  l1 <- digit
  l2 <- digit
  l3 <- digit
  l4 <- digit
  return $ toInt [l1, l2, l3, l4]

parseDashed :: Parser PhoneNumber
parseDashed = do
  area <- parseArea
  _ <- char '-'
  exchange <- parseExchange
  _ <- char '-'
  lineNum <- parseLineNumber
  return $ PhoneNumber area exchange lineNum

parseNoDash :: Parser PhoneNumber
parseNoDash = do
  area <- parseArea
  exchange <- parseExchange
  lineNum <- parseLineNumber
  return $ PhoneNumber area exchange lineNum

parseParens :: Parser PhoneNumber
parseParens = do
  _ <- char '('
  area <- parseArea
  _ <- char ')' >> char ' '
  exchange <- parseExchange
  _ <- char '-'
  lineNum <- parseLineNumber
  return $ PhoneNumber area exchange lineNum

parseExtraOne :: Parser PhoneNumber
parseExtraOne = do
  _ <- char '1' >> char '-'
  rest <- parseDashed
  return rest

parsePhone :: Parser PhoneNumber
parsePhone =
      try parseDashed
  <|> try parseNoDash
  <|> try parseParens
  <|> parseExtraOne

main :: IO ()
main = do
  print $ parseS parsePhone "123-456-7890"
  print $ parseS parsePhone "1234567890"
  print $ parseS parsePhone "(123) 456-7890"
  print $ parseS parsePhone "1-123-456-7890"
