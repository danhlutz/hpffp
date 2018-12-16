-- chapter exercises 1
-- ch24/ex04.hs

module Exercise where

import Control.Applicative ((<|>))
import Text.Trifecta

psv = parseString parseSemVer mempty

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  SemVer a b c _ _ <= SemVer a' b' c' _ _ =
    (a, b, c) <= (a', b', c')

parseSemVer :: Parser SemVer
parseSemVer =
      try parseComplex
  <|> parseSimple

parseSimple :: Parser SemVer
parseSimple = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return $ SemVer major minor patch [] []

parseComplex :: Parser SemVer
parseComplex = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  _ <- char '-'
  (release, meta) <- parseReleaseMeta
  return $ SemVer major minor patch release meta

parseReleaseMeta :: Parser (Release, Metadata)
parseReleaseMeta =
      try parseBothReleaseMeta
  <|> parseReleaseOnly

parseBothReleaseMeta :: Parser (Release, Metadata)
parseBothReleaseMeta = do
  releases <- parseNOSList
  _ <- char '+'
  meta <- parseNOSList
  return (releases, meta)

parseReleaseOnly :: Parser (Release, Metadata)
parseReleaseOnly = do
  releases <- parseNOSList
  return (releases, [])

parseNOSList :: Parser [NumberOrString]
parseNOSList =
      try parseManyNOS
  <|> parseOneNOS

parseOneNOS :: Parser [NumberOrString]
parseOneNOS = do
  item <- parseNOS
  return [item]

parseNOS :: Parser NumberOrString
parseNOS =
      NOSI <$> integer
  <|> NOSS <$> (some (letter <|> digit))

parseManyNOS :: Parser [NumberOrString]
parseManyNOS = do
  first <- parseNOS
  _ <- char '.'
  rest <- parseNOSList
  return $ first : rest
