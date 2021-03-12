module IPv6 where

import Data.List (intersperse, intercalate)
import Data.List.Split (splitOn)
import Text.Trifecta
import Data.Word

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

addressParser :: Parser IPAddress6
addressParser = do
  one <- hex 7
  char ':'
  two <- hex 6
  char ':'
  three <- hex 5
  char ':'
  four <- hex 4
  char ':'
  five <- hex 3
  char ':'
  six <- hex 2
  char ':'
  seven <- fromInteger <$> hexadecimal
  char ':'
  eight <- fromInteger <$> hexadecimal
  return $ IPAddress6
    (one + two + three + four)
    (five + six + seven + (seven * (65535 * 1)) + eight)

hex n =
  (\p -> p + (p * 65535 * n) + (p * (65535 ^ n)))
  . fromInteger <$> hexadecimal

parse =
  parseString addressParser mempty
  . expand

test =
  parse "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

-- Expanding a shortened address to a canonical one

expand :: String -> String
expand =
  (\out -> 'x' : out)
  . intercalate ":x"
  . fmap (intercalate ":x")
  . addZeros
  . fmap (splitOn ":")
  . splitOn "::"

addZeros :: [[String]] -> [[String]]
addZeros components =
  filterComponents
  . intersperse (take nToAdd zeros)
  $ components
  where
    filterComponents = filter ((/=) [""])
    nOfComponents = length . concat . filterComponents $ components
    nToAdd = 8 - nOfComponents

zeros :: [String]
zeros =
  repeat "0000"
