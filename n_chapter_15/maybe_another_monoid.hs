module MaybeAnotherMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
  
instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    sample <- arbitrary
    oneof [ return (First' { getFirst' = Nada }),  return (First' { getFirst' = Only sample })]

instance Monoid (First' a) where
  mempty = First' { getFirst' = Nada }

instance Semigroup (First' a) where
  (<>) (First' Nada) a = a
  (<>) a  (First' Nada) = a
  (<>) a _ = a

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a