module ComposeInstances where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
    foldMap toMonoid (Compose foldable) = (foldMap . foldMap) toMonoid foldable

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse toApplicative (Compose traversable) = Compose <$> (traverse . traverse) (toApplicative) traversable
