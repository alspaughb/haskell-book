-- Chapter 17: Applicative
-- List Applicative Exercise (pages 718-720)
module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- Nil <*> _ = Nil
-- _ <*> Nil = Nil
-- (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = mkList <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = (take' 3000 xs) `eq` (take' 3000 ys)

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

mkList :: [a] -> List a
mkList = foldr Cons Nil

main :: IO ()
main = do
  putStrLn "List Tests:"
  let listTrigger = undefined :: List (String, Char, Int)
  quickBatch $ applicative listTrigger
