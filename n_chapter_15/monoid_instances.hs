module MonoidInstances where

import Test.QuickCheck


-- Trivial

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

trivialChecks :: IO ()
trivialChecks = do
  putStrLn "~~~~~ trivialChecks"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity (a <> a')
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    someA <- arbitrary
    return (Identity someA)

identityChecks :: IO ()
identityChecks = do
  putStrLn "~~~~~ identityChecks"
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)

type IdentityAssoc a =
  Identity a -> Identity a -> Identity a -> Bool

-- Two

data Two a b =
  Two a b
  deriving (Eq, Show)
  
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    someA <- arbitrary
    someB <- arbitrary
    return (Two someA someB)

twoChecks :: IO ()
twoChecks = do
  putStrLn "~~~~~ twoChecks"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] [Int] -> Bool)

type TwoAssoc =
  Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool

-- Mem

newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem left <> Mem right =
    Mem (combineOutputs . runRightAndKeepPreviousResult . left)
    where
      runRightAndKeepPreviousResult (a,s) = (a, right s)
      combineOutputs (a, (a', s')) = (a <> a', s')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

checkMem :: IO ()
checkMem = do
  let
    rmzero = runMem mempty 0
    rmleft = runMem (f' <> mempty) 0
    rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-- Tests

main :: IO ()
main = do
  trivialChecks
  identityChecks
  twoChecks

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
