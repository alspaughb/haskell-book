-- Chapter 15: Monoid, Semigroup
module Main where

import Test.QuickCheck

-- Exercise: Optional Monoid (page 586-587)
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only x) <> Nada     = Only x
  Nada     <> (Only x) = Only x
  (Only x) <> (Only y) = Only (x <> y)
  _        <> _        = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- Madness exercise (pages 593-594)
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbinBetter' e adv noun adj = 
  mconcat [ e , "! he said "
          , adv, " as he jumped into his car "
          , noun, " and drove off with his "
          , adj, " wife."]

-- Exercise: Maybe Another Monoid (pages 599-601)
newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' (Only a)) <> _     = First' (Only a)
  _                 <> other = other

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return $ First' Nada)
              , (3, return $ First' (Only a))]

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)