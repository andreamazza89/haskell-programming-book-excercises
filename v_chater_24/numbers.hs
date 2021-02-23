module Numbers where

import Text.Trifecta
import Control.Applicative

parseDigit :: Parser Char
parseDigit =
  char '0'
  <|> char '1'
  <|> char '2'
  <|> char '3'
  <|> char '4'
  <|> char '5'
  <|> char '6'
  <|> char '7'
  <|> char '8'
  <|> char '9'

base10Integer :: Parser Integer
base10Integer =
  read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' =
  (char '-' >> (negate <$> base10Integer)) <|> base10Integer
