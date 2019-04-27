{-# LANGUAGE FlexibleInstances #-}
-- Chapter 16: Functor
-- Chapter exercises (pages 667-670)
module Chapter where

-- Determine if a valid Functgor can be written
-- for the datatype provided:
-- Question 1: No: Bool :: *
-- Question 2: Yes: BoolAndSomethingElse a :: * -> *
-- Question 3: Yes: BoolAndMaybeSomethingElse a :: * -> *
-- Question 4: Yes: Mu f :: * -> *
-- Question 5: No: D :: *

-- Rearrange the arguments to the type constructor so the
-- Functor instance works
-- Question 1:
data Sum b a = 
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b

-- Question 2:
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- Question 3:
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes
-- Question 1: Quant a b
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- Question 2: K
data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = K x

-- Question 3: Flip f a b
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

-- Question 4: EvilGoateeConst a b
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- Question 5: LiftItOut f a
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- Question 6: Parappa f g a
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- Question 7: IgnoreOne f g a b
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- Question 8: Notorious g o a t
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- Question 9: List a
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Question 10: GoatLord a
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- Question 11: TalkToMe a
data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read (f . g)
