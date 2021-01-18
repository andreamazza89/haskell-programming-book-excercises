module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.List as List

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup key =
  fmap snd . List.find ((== key) . fst)

xs :: Maybe Integer
xs = myLookup 3 $ zip x y

ys :: Maybe Integer
ys = myLookup 6 $ zip y z

zs :: Maybe Integer
zs = myLookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = myLookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  -- solutions
  print $ foldMap (Data.Monoid.All) $ sequA 55
  print $ sequA $ fromMaybe 42 s'
  print $ bolt $ fromMaybe 42 ys

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)