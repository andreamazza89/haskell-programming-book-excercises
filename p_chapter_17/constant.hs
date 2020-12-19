module Constant where

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant c) = Constant c

instance Monoid a => Applicative (Constant a) where
  pure c = Constant mempty
  (<*>) (Constant c) (Constant c') = Constant (c <> c')