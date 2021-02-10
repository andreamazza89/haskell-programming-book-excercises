module UnitOfSuccess where

import Text.Trifecta

main :: IO ()
main =
  print $
    parseString integerParser mempty "123"

integerParser :: Parser Integer
integerParser =
  (integer >>=
    (\int -> eof >> return int)
  )