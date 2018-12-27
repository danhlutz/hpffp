-- ch24/ex07.hs

{-# LANGUAGE QuasiQuotes #-}

module Exercise where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Text.RawString.QQ
import Text.Trifecta

-- steps
-- string -> RawEntry Description StartMinute
-- [RawEntry] -> Entry Description (Maybe StartMinute) (Maybe EndMinute)
-- Calculate total time per Description
-- Calculate average time per Description


testEntry :: String
testEntry = "09:00 Sanitizing moisture collector"

testEntryWithComment :: String
testEntryWithComment =
  "08:00 Breakfast -- should I try skipping breakfast?"

testComment :: String
testComment = "-- a comment!"

testEntries :: String
testEntries = [r|
08:00 Breakfast
09:00 Sanitizing the moisture collector -- FUN
11:00 Exercising in high-grav gym
|]

testDay :: String
testDay = [r|
-- start with a comment!

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing the moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
|]

theLog :: String
theLog = [r|
-- whee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing the moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep


# 2025-02-07 -- not necessarily in sequential order
08:00 Breakfast -- should I try skipping breakfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commuting home in rover
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- digsToInt

digsToInt :: String -> Integer
digsToInt str = go 0 str
  where go :: Integer -> String -> Integer
        go sofar "" = sofar
        go sofar (n:ns) = go (sofar * 10 + (charToInt n)) ns

charToInt :: Char -> Integer
charToInt c =
  case (M.lookup c digs) of
    (Just x) -> x
    Nothing  -> 0
  where digs =
          M.fromList $ zip ['0'..'9'] [0..9]

-- RawEntry
type Description = String
type RawStart = Integer

data RawEntry =
  RawEntry Description RawStart
  deriving (Eq, Show)

mkRawEntry :: String -> String -> String -> RawEntry
mkRawEntry description hour minute =
  RawEntry
    (removeTrailingSpace description)
    ((digsToInt hour) * 60 + (digsToInt minute))

removeTrailingSpace :: String -> String
removeTrailingSpace str =
  if (head $ reverse str) == ' '
  then popEnd str
  else str

popEnd :: String -> String
popEnd str =
  reverse $ tail (reverse str)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char '-'
               skipMany (noneOf "\n")
               skipEOL)

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  skipMany (char ' ' <|> char '\n')

parseHOM :: Parser String
parseHOM = do
  a <- digit
  b <- digit
  return [a, b]

parseEntry :: Parser RawEntry
parseEntry =
      try parseEntryWithComment
  <|> parseSimpleEntry

symsNoDash :: String
symsNoDash = " #?!.,%&"

syms :: String
syms = '-' : symsNoDash

parseSimpleEntry :: Parser RawEntry
parseSimpleEntry = do
  skipEOL
  hour <- parseHOM
  _ <- char ':'
  minute <- parseHOM
  _ <- char ' '
  description <- some (letter <|> digit <|> (oneOf syms))
  return $ mkRawEntry description hour minute

parseEntryWithComment :: Parser RawEntry
parseEntryWithComment = do
  skipEOL
  hour <- parseHOM
  _ <- char ':'
  minute <- parseHOM
  _ <- char ' '
  description <- some (letter <|> digit <|> (oneOf symsNoDash))
  _ <- char '-'
  _ <- char '-'
  _ <- some (letter <|> digit <|> (oneOf syms))
  return $ mkRawEntry description hour minute

parseOneEntry :: Parser [RawEntry]
parseOneEntry = do
  item <- parseEntry
  return [item]

parseManyEntries :: Parser [RawEntry]
parseManyEntries = do
  first <- parseEntry
  _ <- char '\n'
  rest <- parseEntries
  return $ first : rest

parseEntries :: Parser [RawEntry]
parseEntries =
      try parseManyEntries
  <|> parseOneEntry

-- Day

type Year = Integer
type Month = Integer
type Date = Integer

data RawDay =
  RawDay Year Month Date [RawEntry]
  deriving (Eq, Show)

data Day =
  Day Year Month Date
  deriving (Eq, Show)

parseFourDigits :: Parser Integer
parseFourDigits = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  return $ digsToInt [a, b, c, d]

parseTwoDigits :: Parser Integer
parseTwoDigits = do
  a <- digit
  b <- digit
  return $ digsToInt [a, b]

parseDay :: Parser RawDay
parseDay = do
  skipWhiteSpace
  skipComments
  _ <- char '#'
  _ <- char ' '
  year <- parseFourDigits
  _ <- char '-'
  month <- parseTwoDigits
  _ <- char '-'
  date <- parseTwoDigits
  _ <- many (notChar '\n')
  entries <- parseEntries
  return $ RawDay year month date entries

parseDays :: Parser [RawDay]
parseDays =
      try parseManyDays
  <|> parseOneDay

parseOneDay :: Parser [RawDay]
parseOneDay = do
  day <- parseDay
  return [day]

parseManyDays :: Parser [RawDay]
parseManyDays = do
  dayOne <- parseDay
  skipWhiteSpace
  skipComments
  rest <- parseDays
  return $ dayOne : rest

--

type Start = Integer
type End = Maybe Integer

data Entry =
  Entry Description Start End
  deriving (Eq, Show)

mkEntry :: RawEntry -> Maybe RawEntry -> Entry
mkEntry (RawEntry d s) Nothing = Entry d s Nothing
mkEntry (RawEntry d s) (Just (RawEntry _ e)) =
  Entry d s (Just e)

timeSpent :: Entry -> Maybe Integer
timeSpent (Entry _ _ Nothing) = Nothing
timeSpent (Entry _ s (Just e)) = Just $ e - s

zipMaybe :: [a] -> [b] -> [(a, Maybe b)]
zipMaybe [] _ = []
zipMaybe (x:xs) [] = (x, Nothing) : (zipMaybe xs [])
zipMaybe (x:xs) (y:ys) = (x, Just y) : (zipMaybe xs ys)

processDay :: RawDay -> [Entry]
processDay (RawDay _ _ _ entries) =
  map mkFromTup zipped
  where mkFromTup :: (RawEntry, Maybe RawEntry) -> Entry
        mkFromTup tup = mkEntry (fst tup) (snd tup)
        zipped :: [(RawEntry, Maybe RawEntry)]
        zipped = zipMaybe entries (tail entries)

processDays :: [RawDay] -> [Entry]
processDays [] = []
processDays (d:ds) = mappend (processDay d) (processDays ds)

addEntry :: Entry
         -> M.Map String (Maybe Integer)
         -> M.Map String (Maybe Integer)
addEntry e@(Entry d _ _) m =
  case lookedUp of
    Nothing -> M.insert d time m
    (Just sofar) -> M.insert d ((+) <$> time <*> sofar) m
  where lookedUp = M.lookup d m
        time = timeSpent e

entryTotals :: [Entry] -> M.Map String (Maybe Integer)
entryTotals = foldr addEntry M.empty

addEntryForAvg :: Entry
               -> M.Map String ((Maybe Integer), Integer)
               -> M.Map String ((Maybe Integer), Integer)
addEntryForAvg e@(Entry d _ _) m =
  case (M.lookup d m) of
    Nothing -> M.insert d (time, 1) m
    Just (sofar, n) ->
      M.insert d (((+) <$> time <*> sofar), n + 1) m
  where time :: Maybe Integer
        time = timeSpent e

getAvg :: ((Maybe Integer), Integer) -> Maybe Double
getAvg (x, y) =
  (/) <$> xD <*> yD
  where xD :: Maybe Double
        xD = maybeFromInteger x
        yD :: Maybe Double
        yD = Just $ fromInteger y

maybeFromInteger :: Maybe Integer -> Maybe Double
maybeFromInteger Nothing = Nothing
maybeFromInteger (Just x) = Just $ fromInteger x

entryAvgs :: [Entry] -> M.Map String (Maybe Double)
entryAvgs es =
  fmap getAvg processed
  where processed = foldr addEntryForAvg M.empty es

--
main :: IO ()
main = do
  let p f i = parseString f mempty i
  -- print $ p parseEntry testEntry
  -- print $ p parseEntry testEntryWithComment
  -- print $ p parseEntries testEntries
  -- print $ p parseDay testDay
  -- print $ p parseDays theLog
  -- print $ processDays <$> (p parseDays theLog)
  print $ entryTotals <$> (processDays <$> (p parseDays theLog))
  print $ entryAvgs <$> (processDays <$> (p parseDays theLog))
