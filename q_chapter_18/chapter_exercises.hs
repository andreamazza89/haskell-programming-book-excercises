module ChapterExercises where
--
--
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Nope

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  (<*>) _ _ = NopeDotJpg
  pure _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- PhhbtEither

data PhhEither b a
  = Left_ a
  | Right_ b
  deriving (Eq, Show)

instance Functor (PhhEither b) where
  fmap _ (Right_ r) = (Right_ r)
  fmap f (Left_ l) = Left_ (f l)

instance Applicative (PhhEither b) where
  pure = Left_
  (<*>) (Left_ f) (Left_ l) = Left_ (f l)
  (<*>) (Right_ r) _ = (Right_ r)
  (<*>) _ (Right_ r) = (Right_ r)

instance Monad (PhhEither b) where
  return = pure
  (>>=) (Right_ r) _ = Right_ r
  (>>=) (Left_ l) f = f l

instance (Eq b, Eq a) => EqProp (PhhEither b a) where
  (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhEither b a) where
  arbitrary = do
    someB <- arbitrary
    someA <- arbitrary
    elements [Left_ someA, Right_ someB]

main = do
  let nope :: Nope (Int, String, Int)
      nope = undefined
      either :: PhhEither String (Int, Int, Int)
      either = undefined
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  quickBatch $ functor either
  quickBatch $ applicative either
  quickBatch $ monad either

--data PhhhbtEither a b =
--    Left' a
--  | Right' b
--  deriving (Eq, Show)
--
--instance Functor (PhhhbtEither a) where
--  fmap f (Left' a) = Left' a
--  fmap f (Right' b) = Right' (f b)
--
--instance Applicative (PhhhbtEither a) where
--  pure = Right'
--  (<*>) (Left' a) _ = Left' a
--  (<*>) _ (Left' a) = Left' a
--  (<*>) (Right' f) (Right' x) = Right' (f x)
--
--instance Monad (PhhhbtEither a) where
--  return = pure
--  (>>=) (Left' a) _  = Left' a
--  (>>=) (Right' b) f = f b
--
--instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbtEither a b) where
--  arbitrary =  do
--    a <- arbitrary
--    b <- arbitrary
--    elements [Left' a, Right' b]
--
--instance (Eq a, Eq b) => EqProp (PhhhbtEither a b) where
--  (=-=) = eq
