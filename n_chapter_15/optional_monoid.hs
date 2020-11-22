module OptionalMonoid where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a
      => Semigroup (Optional a) where
  (<>) (Only left) (Only right) = Only (left <> right)
  (<>) left@(Only  _) Nada = left
  (<>) Nada right@(Only  _) = right
  (<>) Nada Nada = Nada
