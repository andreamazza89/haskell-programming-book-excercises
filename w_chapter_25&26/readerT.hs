module ReaderT where


newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT r) = ReaderT $ (fmap . fmap) f r

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  (ReaderT fab) <*> (ReaderT a) = ReaderT $ (<*>) <$> fab <*> a

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (ReaderT rm) >>= f =
      ReaderT $ \r -> do
        a <- rm r
        runReaderT (f a)  r