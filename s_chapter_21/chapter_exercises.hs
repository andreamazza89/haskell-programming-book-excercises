module ChapterExercises where

{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap toMonoid (Identity x) = toMonoid x

instance Traversable Identity where
  traverse toApplicative (Identity x) =
    Identity <$> toApplicative x
  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    someA <- arbitrary
    return $ Identity someA

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = (Constant x)

instance Foldable (Constant a) where
  foldr _ accumulator _ = accumulator

instance Traversable (Constant a) where
  sequenceA (Constant x) =
    pure (Constant x)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    someA <- arbitrary
    return $ Constant someA

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

-- Maybe

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap toMonoid (Yep x) = toMonoid x

instance Traversable Optional where
  traverse mapToApplicative Nada = pure Nada
  traverse mapToApplicative (Yep x) = Yep <$> mapToApplicative x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    someA <- arbitrary
    elements [Nada, Yep someA]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

-- List

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap toMonoid (Cons x xs) = toMonoid x <> foldMap toMonoid xs

instance Traversable List where
  traverse mapToApplicative Nil = pure Nil
  traverse mapToApplicative (Cons x xs) = Cons <$> mapToApplicative x <*> traverse mapToApplicative xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    someA <- arbitrary
    someOtherA <- arbitrary
    elements [Cons someA (Cons someOtherA Nil), Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- Three

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap toMonoid (Three a b c) = toMonoid c

instance Traversable (Three a b) where
  traverse mapToApplicative (Three a b c) = Three a b <$> mapToApplicative c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    someC <- arbitrary
    return $ Three someA someB someC

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair | not going to do this one as I think it's pretty much gonna be the same as Three

-- Big, then updated it to be Bigger

data Big a b =
  Big a b b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b' b'') = Big a (f b) (f b') (f b'')

instance Foldable (Big a) where
  foldMap toMonoid (Big a b b' b'') = toMonoid b <> toMonoid b' <> toMonoid b''

instance Traversable (Big a) where
  traverse mapToApplicative (Big a b b' b'') =
    Big a
    <$> mapToApplicative b
    <*> mapToApplicative b'
    <*> mapToApplicative b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    someOtherB <- arbitrary
    yetAnotherB <- arbitrary
    return $ Big someA someB someOtherB yetAnotherB

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Tree

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node ltree x rtree) = Node (fmap f ltree) (f x) (fmap f rtree)

instance Foldable Tree where
  foldMap toMonoid Empty = mempty
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node ltree x rtree) = foldMap toMonoid ltree <> toMonoid x <> foldMap toMonoid rtree

instance Traversable Tree where
  traverse toApplicative Empty = pure Empty
  traverse toApplicative (Leaf x) = Leaf <$> toApplicative x
  traverse toApplicative (Node ltree x rtree) =
    Node
    <$> traverse toApplicative ltree
    <*> toApplicative x
    <*> traverse toApplicative rtree

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    someA <- arbitrary
    someOtherA <- arbitrary
    yetAnotherA <- arbitrary
    elements [Empty, Leaf someA, Node (Leaf someA) someOtherA (Leaf yetAnotherA)]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

tests :: IO ()
tests = do
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Three Int Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Big Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
