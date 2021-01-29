module RandomExample where

import System.Random

-- Six-sided die

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

dieToInt :: Die -> Int
dieToInt DieOne = 1
dieToInt DieTwo = 2
dieToInt DieThree = 3
dieToInt DieFour = 4
dieToInt DieFive = 5
dieToInt DieSix = 6

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x ->
      error $
        "intToDie got non 1-6 integer: " ++ show x
