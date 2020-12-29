module ChapterExercises where

data Constant a b =
  Constant b
  
instance Foldable (Constant a) where
  foldMap toMonoid (Constant x) = toMonoid x

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap toMonoid (Two _ x) = toMonoid x

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap toMonoid (Three _ _ x) = toMonoid x

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap toMonoid (Three' _ x x') = toMonoid x <> toMonoid x'

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap toMonoid (Four' _ x x' x'') = mconcat . map toMonoid $ [x, x', x'']

filterF :: ( Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF shouldKeep =
  foldMap (\item -> if shouldKeep item then pure item else mempty)
