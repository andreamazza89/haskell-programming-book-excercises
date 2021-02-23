module Logs where

import Control.Applicative
import Data.Map
import qualified Text.Trifecta as Trife

-- Logs

data Logs =
  Logs (Map Date Entries)
  deriving (Show)

-- Date

data Date =
  Date Year Month Day
  deriving (Eq, Show)

type Year = Integer
type Month = Integer
type Day = Integer

instance Ord Date where
  (Date year month day) <= (Date year' month' day') =
    year <= year' && month <= month' && day <= day'

-- Entries

type Entries =
  [Entry]

data Entry =
  Entry Time Activity
  deriving (Show)

type Activity = String

data Time =
  Time Hours Minutes
  deriving (Show)

type Hours = Integer
type Minutes = Integer

-- Parser

parseLogs :: String -> Trife.Result Logs
parseLogs rawLogs =
  Trife.parseString logsParser mempty rawLogs

logsParser :: Trife.Parser Logs
logsParser = do
  Trife.many whiteSpaceOrComment
  logs <- Trife.some logParser
  return $ Logs (fromList logs)

whiteSpaceOrComment :: Trife.Parser ()
whiteSpaceOrComment =
  (Trife.char ' ' >> return ())
    <|> (Trife.symbol "\n" >> return ())
    <|> (commentParser >> return ())

commentParser :: Trife.Parser String
commentParser = do
  Trife.string "-- "
    >> Trife.some (Trife.letter <|> Trife.char ' ')

logParser :: Trife.Parser (Date, Entries)
logParser = do
  date <- dateParser
  Trife.some (whiteSpaceOrComment)
  entries <- Trife.sepEndBy entryParser (Trife.some whiteSpaceOrComment)
  return $ (date, entries)

entryParser :: Trife.Parser Entry
entryParser = do
  time <- timeParser
  activity <- Trife.some (Trife.letter <|> Trife.char ' ')
  return $ Entry time activity

timeParser :: Trife.Parser Time
timeParser = do
  hours <- read <$> Trife.count 2 Trife.digit
  Trife.char ':'
  minutes <- read <$> Trife.count 2 Trife.digit
  Trife.char ' '
  return $ Time hours minutes

dateParser :: Trife.Parser Date
dateParser = do
  Trife.string "# "
  year <- read <$> Trife.count 4 Trife.digit
  Trife.char '-'
  month <- read <$> Trife.count 2 Trife.digit
  Trife.char '-'
  day <- read <$> Trife.count 2 Trife.digit
  return $ Date year month day

test =
  parseLogs "# 2025-01-01 \n -- and a comment here\n10:42 Test entry \n# 2020-05-05 \n20:30 Ate Brekkie\n20:33 Did this"