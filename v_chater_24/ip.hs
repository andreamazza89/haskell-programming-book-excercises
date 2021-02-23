module Ip where

import Data.Word
import Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parse =
  parseString parser mempty "204.120.0.15"

parser :: Parser IPAddress
parser = do
  one <- toBinString <$> some digit
  char '.'
  two <- toBinString <$> some digit
  char '.'
  three <- toBinString <$> some digit
  char '.'
  four <- toBinString <$> some digit
  return $ IPAddress $ binaryStringToDecimal (one ++ two ++ three ++ four)

toBinString :: String -> String
toBinString =
  padEight . decimalToBinaryString . read


padEight :: String -> String
padEight string =
  pad ++ string
  where pad = take (8 - (length string)) zeroes
        zeroes = repeat '0'

-- to and from baseTwo

decimalToBinaryString :: Integer -> String
decimalToBinaryString dec =
  go dec ""
  where go n result
          | n == 0 = result
          | otherwise = go (n `div` 2) (show (n `mod` 2) ++ result)

binaryStringToDecimal :: String -> Word32
binaryStringToDecimal binary =
  sum . fmap (\(mul, digit) -> mul * digit) . zip powerOfTwo . fmap charToInt . reverse $ binary

charToInt :: Char -> Word32
charToInt c =
  read [c]

powerOfTwo :: [Word32]
powerOfTwo = (\pow -> 2 ^ pow) <$> [0..]
