-- Chapter 16: Functor
module Instances where
import Test.QuickCheck

-- Exercises: Instances of Func
-- Question 1: Identity a
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = do
     a <- arbitrary
     return (Identity a)

type IdentityComp a = 
     (Identity a)
  -> (Fun a a) 
  -> (Fun a a) 
  -> Bool

-- Question 2: Pair a
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

type PairComp a =
     (Pair a)
  -> (Fun a a)
  -> (Fun a a)
  -> Bool

-- Question 3: Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => 
    Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoComp a b =
     (Two a b)
  -> (Fun b b)
  -> (Fun b b)
  -> Bool

-- Question 4: Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type ThreeComp a b c =
     (Three a b c)
  -> (Fun c c)
  -> (Fun c c)
  -> Bool

-- Question 5: Three' a b
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

type Three'Comp a b =
     (Three' a b)
  -> (Fun b b)
  -> (Fun b b)
  -> Bool

-- Question 6: Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

type FourComp a b c d =
     (Four a b c d)
  -> (Fun d d)
  -> (Fun d d)
  -> Bool

-- Question 7: Four' a b
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

type Four'Comp a b =
     (Four' a b)
  -> (Fun b b)
  -> (Fun b b)
  -> Bool

-- Question 8: Trivial
-- Can't be implemented because Trivial is not a higher-kinded type

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose :: IdentityComp Int)

  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose :: PairComp Int)

  quickCheck (functorIdentity :: (Two Char Int) -> Bool)
  quickCheck (functorCompose :: TwoComp Char Int)

  quickCheck (functorIdentity :: (Three Char Int Bool) -> Bool)
  quickCheck (functorCompose :: ThreeComp Char Int Bool)

  quickCheck (functorIdentity :: (Three' Char Int) -> Bool)
  quickCheck (functorCompose :: Three'Comp Char Int)

  quickCheck (functorIdentity :: (Four Char Int Bool Double) -> Bool)
  quickCheck (functorCompose :: FourComp Char Int Bool Double)

  quickCheck (functorIdentity :: (Four' Char Int) -> Bool)
  quickCheck (functorCompose :: Four'Comp Char Int)
