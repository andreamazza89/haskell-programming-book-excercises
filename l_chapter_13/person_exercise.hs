module PersonExercise where

type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Hi, please enter a name:"
  name <- getLine
  putStrLn "now enter an age:"
  ageAsString <- getLine
  case mkPerson name (read ageAsString) of
    Right person ->  putStrLn $ "yes: " ++ (show person)
    Left _ ->  putStrLn "yo, no bueno"
  return ()