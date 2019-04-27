-- Chapter 16: Functor
module Exercises where

-- Exercises: Be Kind (page 623)
-- Question 1: a :: *
-- Question 2: b :: * -> *
--             T :: * -> *
-- Question 3: c :: * -> * -> *

-- Exercises: Heavy Lifting (pages 645-646)
-- Question 1:
a = fmap (+1) $ read "[1]" :: [Int]
-- Question 2:
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Question 3:
c = (*2) . (\x -> x - 2)
-- Question 4:
d = 
  ((return '1' ++) . show) .
  (\x -> [x, 1..3])
-- Question 5:
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- Exercise: Possibly (page 654)
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ LolNope     = LolNope

-- Short Exercise (page 656)
-- Question 1: Sum a b
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

