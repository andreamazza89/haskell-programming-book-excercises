module SomeInstances where

import Control.Monad.IO.Class

-- maybs

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= f = MaybeT $ do
    a <- ma
    case a of
      Just a_ -> runMaybeT $ f a_
      Nothing -> return Nothing

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = MaybeT . fmap Just . liftIO

-- reader

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

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO


-- state

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

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = StateT . (\m -> (\s -> fmap (\a -> (a, s)) m)) . liftIO
