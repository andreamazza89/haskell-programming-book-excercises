module StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT s) = StateT $ (fmap . fmap) (mapFst f) s

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, s)  =
  (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT mf) <*> (StateT ma) =
    StateT $ \s -> do
      (fab, s') <- mf s
      (a, s'') <- ma s'
      return (fab a, s'')


instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
