{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g a b =
  g <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f r = pure f <*> r


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a =
    Reader $ (\r -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ rab <*> ra

instance Monad (Reader r) where
  return :: a -> Reader r a
  return =
    pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) r
