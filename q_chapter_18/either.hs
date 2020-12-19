module EitherExercise where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (Second f) (Second x) = Second (f x)
  (<*>) (First x) _ = First x
  (<*>) _ (First x) = First x

instance Monad (Sum a) where
  return = pure
  (>>=) (Second x) f = f x
  (>>=) (First x) _ = First x
