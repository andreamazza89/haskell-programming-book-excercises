module SemiGroupInstances where

import Test.QuickCheck

-- Trivial

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Show)

  --instance Semigroup (Identity a) where
  --  Identity left <> Identity right = Identity right
    -- this works but feels weird to wholesale discard one of the values; the one below 'feels' better

instance Semigroup a => Semigroup (Identity a) where
  Identity left <> Identity right = Identity (left <> right)

-- Two

data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

-- Three

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

-- BoolConj

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj left <> BoolConj right = BoolConj (left && right)

-- Or

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  getIt@(Snd _) <> _ = getIt
  _ <> getIt@(Snd _) = getIt
  Fst _ <> getIt@(Fst _) = getIt

-- Combine

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)



data Validation a b =
    MyFailure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  MyFailure a <> MyFailure a' = MyFailure (a <> a')
  MyFailure a <> _ = MyFailure a
  _ <> MyFailure a  = MyFailure a

-- Tests

main :: IO ()
main =
  do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    someA <- arbitrary
    return (Identity someA)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    return (Two someA someB)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    someC <- arbitrary
    return (Three someA someB someC)

instance Arbitrary BoolConj where
  arbitrary = oneof . map return $  [BoolConj True, BoolConj False]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    oneof . map return $ [Fst someA, Snd someB]

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc =
  Identity [Int] -> Identity [Int] ->  Identity [Int] -> Bool

type TwoAssoc =
  Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool

type ThreeAssoc =
  Three [Int] [Int] [Int]-> Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Bool

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

type OrAssoc =
  Or [Int] [Int] -> Or [Int] [Int] -> Or [Int] [Int] -> Bool

