-- Chapter 17: Applicatives
module Exercises where

import Data.List (elemIndex)
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (applicative)

-- Exercises: Lookups (pages 689-690)
-- Question 1:
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- Question 2:
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [1, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [1, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- Question 3:
x1 :: Maybe Int
x1 = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

-- Question 4:
xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x2 <*> y2

-- Exercises: Identity Instance (page 692)
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

-- Exercise: Constant Instance (pages 693-694)
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a
      => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)

-- Exercise: Fixer Upper (page 707)
-- Question 1:
fixerUpper1 = const <$> Just "Hello" <*> pure "World"

-- Question 2:
fixerUpper2 = (,,,) <$> Just 90
  <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise (pages 718-720)
-- See: ListApplicative.hs

-- ZipList Applicative Exercise (pages 720-722)
-- See: ZipListApplicative.hs

-- Exercise: Variations on Either (page 725)
data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success

  (Success f) <*> (Success a)  = Success (f a)
  (Failure e) <*> (Failure e') = Failure (e <> e')
  (Failure e) <*> _            = Failure e
  _           <*> (Failure e)  = Failure e

instance (Arbitrary e, Arbitrary a) => 
    Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return (Success a)), (1, return (Failure e))]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Validation [String] (String, Char, Int)
  quickBatch (applicative trigger)

-- Chapter Exercises (pages 725-727)
-- See Chapter.hs
