{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

-- Rearrange

data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
      L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)

data K a b =
  K a
  deriving Show

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a
  deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip (K' (f x))

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut functor) = LiftItOut (fmap f functor)

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa functorA functorB) = DaWrappa (fmap f functorA) (fmap f functorB)


data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)


data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read x) = Read (fmap f x)
