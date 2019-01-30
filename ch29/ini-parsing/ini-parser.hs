-- ch24-05.hs

module Data.Ini where

import Control.Applicative
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory (listDirectory)
import Text.Trifecta

newtype Header =
  Header String
  deriving (Eq, Show, Ord)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhiteSpace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return (Config mapOfSections)

-- added to work with file IO as exercise in Chapter 29

type ConfigMap =
  Map FilePath Config

addConfig :: FilePath -> IO ConfigMap -> IO ConfigMap
addConfig fp m = do
  m' <- m
  contents <- readFile fp
  let (Success parsed) = parseString parseIni mempty contents
      newmap = M.insert fp parsed m'
  return newmap

emptyCF :: IO ConfigMap
emptyCF = return M.empty

isIni :: FilePath -> Bool
isIni []     = False
isIni ".ini" = True
isIni (_:xs) = isIni xs

getIniFiles :: FilePath -> IO [FilePath]
getIniFiles d = do
  files <- listDirectory d
  let iniFiles = filter isIni files
  case d of
    "." -> return iniFiles
    _   -> return $ fmap (d++) iniFiles

addConfigs :: FilePath -> IO ConfigMap
addConfigs fp = do
  files <- getIniFiles fp
  foldr addConfig emptyCF files
