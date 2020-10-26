module WriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigitA :: Integral a => a -> a
tensDigitA x = d
  where (_, d) = x `divMod` 10

hundredsDigitC :: Integral a => a -> a
hundredsDigitC =
  snd . (\x -> x `divMod` 10) . (\x -> x `div` 100)


foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y bool =
  case bool of
    True -> y
    False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool
  | bool == True = y
  | bool == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
