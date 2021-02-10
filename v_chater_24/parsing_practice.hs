module ParsingPractice where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative

one :: Parser ()
one = char '1' >> eof

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

testParse :: Parser () -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

exOne :: IO ()
exOne = do
  pNL "one:"
  testParse one
  pNL "oneTwo:"
  testParse oneTwo

-- ex 2

oneTwoThree :: Parser String
oneTwoThree = myString "123" <|>  string "12" <|> string "1"

testParseTwo :: Parser String -> IO ()
testParseTwo p =
  print $ parseString p mempty "123"

exTwo :: IO ()
exTwo = do
  pNL "one:"
  testParseTwo oneTwoThree


-- ex 3

myString :: String -> Parser String
myString [] = return []
myString (x:xs) = char x >>= (\ch -> (ch :) <$> (myString xs))
