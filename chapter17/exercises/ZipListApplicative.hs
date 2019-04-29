-- Chapter 17: Applicative
-- ZipList Applicative Exercise (Pages 720-722)
module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (ZipList' fs) <*> (ZipList' xs) =
    ZipList' $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  (ZipList' xs) =-= (ZipList' ys) =
    (take 3000 xs) `eq` (take 3000 ys)

main :: IO ()
main = do
  putStr "ZipList' Tests:"
  let zipListTrigger = undefined :: ZipList' (String, Char, Int)
  quickBatch (applicative zipListTrigger)
