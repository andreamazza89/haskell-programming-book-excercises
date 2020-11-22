module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

type IntToInt = Fun Int Int

main = do
  checkIdentity
  checkPair
  checkTwo
  checkThree'

-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    someA <- arbitrary
    return (Identity someA)

checkIdentity = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)

-- Pair

data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair left right) = Pair (f left) (f right)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    return (Pair someA someB)

checkPair = do
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)

-- Two

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    return (Two someA someB)

checkTwo = do
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)

--  Three'

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    someA  <- arbitrary
    someB  <- arbitrary
    someB' <- arbitrary
    return (Three' someA someB someB')

checkThree' = do
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)

-- Possibly

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap f LolNope = LolNope

-- Property test definitions

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) =>
     f a
  -> Fun a b
  -> Fun b c
  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)