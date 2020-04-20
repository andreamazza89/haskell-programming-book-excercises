{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where -- simple example

example = 1

one = (* 9) 6

c =  head [(0 :: Integer ,"doge"),(1,"kitteh")]

d = if False then True else False

e = length [1, 2, 3, 4, 5]

f = (length [1, 2, 3, 4]) > (length "TACOCAT")
