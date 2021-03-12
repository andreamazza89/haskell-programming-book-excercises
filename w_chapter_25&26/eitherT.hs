module EitherT where

import Control.Monad

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me) =
      EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a


instance Monad m => Monad (EitherT e m) where
  return = pure

  (EitherT v) >>= f =
    EitherT $ v >>= either (return . Left) (runEitherT . f)

swapEither :: Either e a -> Either a e
swapEither = either Right Left

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT m) = EitherT $ fmap swapEither m

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT left right (EitherT m) = m >>= either left right
