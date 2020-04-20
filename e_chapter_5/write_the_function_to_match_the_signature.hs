module WriteTheFunction where

i :: a -> a
i = id

c :: a -> b -> a
c x y = x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC $ aToB x
