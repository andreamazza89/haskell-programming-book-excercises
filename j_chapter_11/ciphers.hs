module Ciphers where

import Data.Char

encode2 :: String -> String -> String
encode2 toEncode keyword =
  doEncode2 toEncode (concat . repeat $ keyword) ""

doEncode2 [] keyword encoded = encoded
doEncode2 (e:toEncode) (k:keyword) encoded =
  if e == ' ' then
    doEncode2 toEncode (k : keyword) (encoded ++ " " )
  else
    doEncode2 toEncode keyword (encoded ++ encodeChar e k )

encodeChar char key =
  [chr ((((adjust char) + (adjust key)) `mod` 26) + 65)]

adjust c =
  (ord c) - 65

test =
  if encode2 "MEET AT DAWN" "ALLY" == "MPPR AE OYWY" then
    putStrLn "ok"
  else
    putStrLn "not ok"
