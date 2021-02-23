module Semver where

import Text.Trifecta
import Control.Applicative

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer

type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show)

instance Eq SemVer where
  (SemVer maj min patch rel meta) == (SemVer maj' min' patch' rel' meta') =
    maj == maj' && min == min' && patch == patch' && rel == rel' && meta == meta'

instance Ord SemVer where
  (SemVer maj min patch _ _) <= (SemVer maj' min' patch' _ _) =
    maj <= maj' && min <= min' && patch <= patch'

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer
    <$> parseMajor
    <* dot_
    <*> parseMinor
    <* dot_
    <*> parsePatch
    <*> parseRelease
    <*> parseMetadata

parseMajor :: Parser Major
parseMajor = integer

parseMinor :: Parser Minor
parseMinor = integer

parsePatch :: Parser Patch
parsePatch = integer

parseRelease :: Parser Release
parseRelease = withDefault (char '-' *> dottedNumbersOrStrings) []

parseMetadata :: Parser Metadata
parseMetadata = withDefault (char '+' *> dottedNumbersOrStrings) []

dottedNumbersOrStrings :: Parser [NumberOrString]
dottedNumbersOrStrings  =
  sepBy numberOrString (symbol ".")

numberOrString :: Parser NumberOrString
numberOrString =
   (NOSS <$> (some letter))
   <|> (NOSI <$> integer)

dot_ :: Parser ()
dot_ =
  char '.' >> return ()

withDefault :: Parser a -> a -> Parser a
withDefault parser defaultValue =
  parser <|> return defaultValue


main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92+1.3.s"
  print $ parseString parseSemVer mempty "1.0.0+f.1.3.s"
  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
