module StringProcessing where

---------

replaceThe :: String -> String
replaceThe input =
   join . fmap swapThe . words $ input

swapThe :: String -> String
swapThe word =
  if word == "the" then
    "a"
  else
    word

join :: [String] -> String
join words =
  trimSpace . foldl accumulateWithSpace [] $ words
  where
    trimSpace = (\(space:result) -> result)
    accumulateWithSpace = (\out word -> out ++ " " ++ word)


----------------

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input =
  countTheBeforeVowel' (words input) 0

countTheBeforeVowel' :: [String] -> Integer -> Integer
countTheBeforeVowel' (a:b:rest) total =
  if (isThe a) && (beginsWithVowel b) then
    countTheBeforeVowel' (b:rest) (total + 1)
  else
    countTheBeforeVowel' (b:rest) total
countTheBeforeVowel' (a:rest) total = total
countTheBeforeVowel' [] total = total

isThe word =
  word == "the"

beginsWithVowel (firstChar:_) =
  isVowel firstChar

isVowel char =
  char == 'a'
  || char == 'e'
  || char == 'i'
  || char == 'o'
  || char == 'u'

----------------------


countVowels :: String -> Integer
countVowels input =
 sum . fmap (\ch -> if isVowel ch then 1 else 0) $ input