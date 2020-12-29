module AdditionalExercises where

-- these are from http://blog.sigfpe.com/2007/04/trivial-monad.html

data W x = W x deriving Show

instance Functor W where
  fmap f (W x) = W (f x)


instance Applicative W where
  pure = W
  W f <*> W x = W (f x)

instance Monad W where
  return = pure
  W x >>= f = f x

-- 1 | Define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y).
--     Obviously that definition won't do - the left hand side has a W y pattern so it's actually unwrapping.
--     Rewrite this function so that the only unwrapping that happens is carried out by bind.

g :: Int -> W Int -> W Int
g number wrappedNumber =
  wrappedNumber >>= return . (+ number)

-- 2 | Define a function h :: W Int -> W Int -> W Int so that h (W x) (W y) = W (x+y). Again, no unwrapping.

h :: W Int -> W Int -> W Int
h wrappedNumber wrappedNumber' =
  wrappedNumber >>= flip g wrappedNumber'

-- 4 | We can't completely unwrap things using the monad API. But we can unwrap one layer from things that are wrapped twice.
--     So here's a nice puzzle: define a function join :: W (W a) -> W a using the Monad API and no explicit unwrapping.

join :: W (W a) -> W a
join doublyWrapped =
  doublyWrapped >>= id