{-# LANGUAGE InstanceSigs #-}

module Identita where

import Control.Monad (join)

-- Identity

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) =
    Identity (f a)

instance Monad Identity where
  return = pure

  (Identity a) >>= f =
    f a

-- IdentityT

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m)
       => Functor (IdentityT m) where

  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance (Applicative m)
       => Applicative (IdentityT m) where

  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance (Monad m)
       => Monad (IdentityT m) where

  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
