module LibraryFunctions where

import Data.Monoid
import Data.Foldable

sum :: (Foldable t, Num a) => t a -> a
sum =
  getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product =
  getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem toMatch =
  getAny . foldMap (Any . (==) toMatch)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
  getMin . foldMap (Min . Just)

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Semigroup (Min a) where
  (<>) (Min (Just x)) (Min (Just x')) = if x <= x' then Min (Just x) else Min (Just x')
  (<>) (Min Nothing) (Min (Just x')) = (Min (Just x'))
  (<>) (Min (Just x)) (Min Nothing) = (Min (Just x))
  (<>) (Min Nothing) (Min Nothing) = (Min Nothing)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing


null :: (Foldable t) => t a -> Bool
null =
  foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length =
  getSum . foldMap (const $ Sum 1)

toList :: (Foldable t) => t a -> [a]
toList =
  foldMap (\x -> [x])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap toMonoid =
  foldr (\item accumulation -> toMonoid item <> accumulation) mempty