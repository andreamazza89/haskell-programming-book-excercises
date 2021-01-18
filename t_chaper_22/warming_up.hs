module WarmingUp where


import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

mTupled :: [Char] -> ([Char], [Char])
mTupled = do
  capitals <- cap
  reversed <- rev
  return (capitals, reversed)

mTupled' :: [Char] -> ([Char], [Char])
mTupled' =
  cap
    >>= (\c -> rev
    >>= (\r ->
      return (c, r)
      )
    )
