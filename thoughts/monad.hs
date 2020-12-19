-- this (from https://en.wikibooks.org/wiki/Haskell/Applicative_functors#A_sliding_scale_of_power) is interesting:

-- (<$>) :: Functor t     =>   (a ->   b) -> (t a -> t b)
-- (<*>) :: Applicative t => t (a ->   b) -> (t a -> t b)
-- (=<<) :: Monad t       =>   (a -> t b) -> (t a -> t b)

-- showing how there is some sort of symmetry between the three; they're all about mapping in the presence of a
-- structure/context, the difference being about whether the mapping is just to the content of the structure (functor)
-- or both (applicative, monad).