module PhoneNumbers where

import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  planArea   <- parseArea
  sep_
  exchange   <- parseExchange
  sep_
  lineNumber <- parseLine
  return $ PhoneNumber planArea exchange lineNumber

parseArea :: Parser NumberingPlanArea
parseArea =
  between (optional (char '(')) (optional (char ')')) parseArea_

parseArea_ :: Parser NumberingPlanArea
parseArea_ =
  read <$> (optional extraBitInFront >> count 3 digit)

extraBitInFront :: Parser Char
extraBitInFront =
  try (digit >> char '-')

parseExchange :: Parser Exchange
parseExchange =
  read <$> count 3 digit

parseLine :: Parser LineNumber
parseLine =
  read <$> count 4 digit

sep_ :: Parser (Maybe Char)
sep_ =
  optional (char '-' <|> char ' ')

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
