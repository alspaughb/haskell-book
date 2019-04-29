-- Chapter 17: Applicative
-- End of chapter exercises
module Chapter where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

-- Specialize the types (pages 725-726)
-- Question 1:
type PureList a = a -> [a]
type ApplyList a b = [(a -> b)] -> [a] -> [b]

-- Question 2:
type PureIO a = a -> IO a
type ApplyIO a b = IO (a -> b) -> IO a -> IO b

-- Question 3:
type PureTuple a b = a -> (b, a)
type ApplyTuple a b c d e = (c, (a -> b)) -> (d, a) -> (e, b)

-- Question 4:
type PureArrow a b = a -> (->) a b
type ApplyArrow a b c d e = (->) c (a -> b) -> (->) d a -> (->) e b

-- Write instances for the following datatypes (page 726)
-- Question 1: Pair a
data Pair a = Pair a a 
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x

  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- Question 2: Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty

  (Two x f) <*> (Two x' y) = Two (x <> x') (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Question 3: Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty

  (Three x y f) <*> (Three x' y' z) = Three (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
    Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Question 4: Three' a b
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x

  (Three' x f g) <*> (Three' x' y z) = Three' (x <> x') (f y) (g z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- Question 5: Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => 
    Applicative (Four a b c) where
  pure = Four mempty mempty mempty

  (Four w x y f) <*> (Four w' x' y' z) = 
    Four (w <> w') (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
    Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- Question 6: Four' a b
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty

  (Four' w x y f) <*> (Four' w' x' y' z) = 
    Four' (w <> w') (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  putStr "Pair Test:"
  let pairTrigger = undefined :: Pair (Int, Int, Int)
  quickBatch (applicative pairTrigger)

  putStr "Two Test:"
  let twoTrigger = undefined :: Two [Int] (Int, Int, Int)
  quickBatch (applicative twoTrigger)

  putStr "Three Test:"
  let threeTrigger = undefined :: Three [Int] [Int] (Int, Int, Int)
  quickBatch (applicative threeTrigger)

  putStr "Three' Test:"
  let threePrimeTrigger = undefined :: Three' [Int] (Int, Int, Int)
  quickBatch (applicative threePrimeTrigger)

  putStr "Four Test:"
  let fourTrigger = undefined :: Four [Int] [Int] [Int] (Int, Int, Int)
  quickBatch (applicative fourTrigger)

  putStr "Four' Test:"
  let fourPrimeTrigger = undefined :: Four' [Int] (Int, Int, Int)
  quickBatch (applicative fourPrimeTrigger)

-- Combinations (page 727)
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = liftA3 (,,) xs ys zs
