-- ch16/ex12.hs

module Exercise where

-- 4)

data EvilGoateeConst a b =
  GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5)
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g)
  => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7
data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g)
  => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething x y) =
      IgnoreSomething x (fmap f y)

-- 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- 9
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) =
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read (fmap f g)
