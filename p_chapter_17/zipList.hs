module ZipList' where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = doTake Nil

doTake :: List a -> Int -> List a -> List a
doTake taken _ Nil = taken
doTake taken 0 _ = taken
doTake taken leftToTake (Cons x xs) = doTake (taken <> Cons x Nil) (leftToTake - 1) xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = (Cons (f x y) Nil) <> (zipWith' f xs ys)
zipWith' f Nil _ = Nil
zipWith' f _ Nil = Nil

instance Semigroup (List a) where
  Nil <> x = x
  Cons x xs <> xs' = Cons x (xs <> xs')

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    someA <- arbitrary
    someOtherA <- arbitrary
    return (Cons someA (Cons someOtherA Nil))

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l


instance Semigroup (ZipList' a) where
  (<>) (ZipList' l) (ZipList' r) = ZipList' (l <> r)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' (zipWith' ($) xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    someList <- arbitrary
    return $ ZipList' someList

main :: IO ()
main = quickBatch (applicative (ZipList' (Cons ("a", "b", (1 :: Int)) Nil)))
