module AsPattern where

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf subsequence sequence =
  case process subsequence sequence of
    [] -> True
    _ -> False

process :: (Eq a) =>  [a] -> [a] -> [a]
process subsequence sequence  =
  foldl match subsequence sequence

match [] toMatch = []
match subsequence@(sub:subs) toMatch =
  if sub == toMatch then
    subs
  else
    subsequence


test =
  if isSubseqOf "blah" "blahwoot" == True then
    putStrLn "ok"
  else
    putStrLn "not ok"
