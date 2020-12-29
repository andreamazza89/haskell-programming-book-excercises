module FinalExercises where

j :: Monad m => m (m a) -> m a
j nestedMonad = nestedMonad >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  ma >>= (\a -> mb >>= return . f a)

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf >>= (\f -> ma >>= return . f)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> (f x >>= return) <*> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType [] = return []
flipType xs = meh xs (\x -> x >>= return . id)
