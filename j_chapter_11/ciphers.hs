module Ciphers where

import Data.Char

encode2 :: String -> String -> String
encode2 toEncode keyword =
  doEncode2 toEncode (concat . repeat $ keyword) ""

doEncode2 :: String -> String -> String -> String
doEncode2 [] keyword encoded = encoded
doEncode2 (e:toEncode) (k:keyword) encoded
  | e == ' ' = doEncode2 toEncode (k : keyword) (encoded ++ " " )
  | otherwise = doEncode2 toEncode keyword (encoded ++ [encodeChar e k] )

encodeChar :: Char -> Char -> Char
encodeChar char key =
  chr
  . ((+) 65)
  . ((flip mod) 26)
  . ((+) (adjust key))
  . adjust
  $ char

adjust c =
  (ord c) - 65

test =
  if encode2 "MEET AT DAWN" "ALLY" == "MPPR AE OYWY" then
    putStrLn "ok"
  else
    putStrLn "not ok"
