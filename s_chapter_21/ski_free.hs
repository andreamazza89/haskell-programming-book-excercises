module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S xs x) = S (f <$> xs) (f x)

instance (Foldable n) => Foldable (S n) where
  foldMap toMonoid (S xs x) = foldMap toMonoid xs <> toMonoid x

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Traversable n => Traversable (S n) where
  traverse toApplicative (S xs x) = S <$> traverse toApplicative xs <*> toApplicative x
