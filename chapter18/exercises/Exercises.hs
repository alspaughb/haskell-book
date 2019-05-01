-- Chapter 18: Monad
module Exercises where

import Control.Monad (ap, join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The answer is the exercise (page 735)
bind :: Monad m => (a -> m b) -> m a -> m b 
bind f m = join $ fmap f m

-- Short Exercise: Either Monad (page 758)
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (<*>) = ap

--  First x  <*> _        = First x
--  _        <*> First x  = First x
--  Second f <*> Second x = Second (f x)

instance Monad (Sum a) where
  return = pure

  First x  >>= _ = First x
  Second x >>= f = f x

-- Chapter Exercises (page 770)
-- Question 1: Nope
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure

  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- Question 2: BahEither
data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft x)  = PLeft $ f x
  fmap _ (PRight x) = PRight x

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) = ap
--  PRight x <*> _        = PRight x
--  _        <*> PRight x = PRight x
--  PLeft f  <*> PLeft x  = PLeft $ f x

instance Monad (BahEither b) where
  return = pure

  PRight x >>= _ = PRight x
  PLeft x  >>= f = f x

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) 
    => EqProp (BahEither b a) where
  (=-=) = eq

-- Question 3: Identity
newtype Identity a = Identity a 
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity

  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure

  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Question 4: List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x    = Cons x Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs

instance Monad List where
  return   = pure
  xs >>= f = flatMap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = mkList <$> arbitrary
    where mkList [] = Nil
          mkList (x:xs) = Cons x $ mkList xs

instance Eq a => EqProp (List a) where
  xs =-= ys = (take' 300 xs) `eq` (take' 300 ys)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil      = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

main :: IO ()
main = do
  putStr "Question 1: Nope"
  let nopeTrigger 
        = undefined :: Nope (Int, String, Int)
  quickBatch $ functor nopeTrigger
  quickBatch $ applicative nopeTrigger
  quickBatch $ monad nopeTrigger

  putStrLn ""
  putStr "Question 2: BahEither"
  let bahEitherTrigger 
        = undefined :: BahEither Int (Int, String, Int)
  quickBatch $ functor bahEitherTrigger
  quickBatch $ applicative bahEitherTrigger
  quickBatch $ monad bahEitherTrigger

  putStrLn ""
  putStr "Question 3: Identity"
  let identityTrigger 
        = undefined :: Identity (Int, String, Int)
  quickBatch $ functor identityTrigger
  quickBatch $ applicative identityTrigger
  quickBatch $ monad identityTrigger

  putStrLn ""
  putStr "Question 4: List"
  let listTrigger 
        = undefined :: List (Int, String, Int)
  quickBatch $ functor listTrigger
  quickBatch $ applicative listTrigger
  quickBatch $ monad listTrigger

-- Write the functions (pages 771-772)
-- Question 1
j :: Monad m => m (m a) -> m a 
j = join

-- Question 2
l1 :: Monad m => (a -> b) -> m a -> m b 
l1 = fmap

-- Question 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

-- Question 4
a :: Monad m => m a -> m (a -> b) -> m b 
a = flip (<*>)

-- Question 5
meh :: Monad m 
    => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (a:as) f = do
  b <- f a
  bs <- meh as f
  return $ b : bs
{-
--meh (a:as) f =
--  (f a) >>= (\b -> (meh as f) >>= (\bs -> return $ b : bs))
-}

-- Question 6
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id

