-- Chapter 20: Foldable
import Data.Monoid

-- Exercises: Library Functions (pages 818-819)
-- Question 1: sum
sum' :: (Foldable t, Num a) => t a -> a
--sum' f = foldr (+) 0 f
sum' f = getSum $ foldMap Sum f

-- Question 2: product
product' :: (Foldable t, Num a) => t a -> a
--product' f = foldr (*) 1 f
product' f = getProduct $ foldMap Product f

-- Question 3: elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
--elem' z f = foldr (\x y -> x == z || y) False f
elem' z f = getAny $ foldMap (\x -> Any (x == z)) f 

-- Question 4: minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just (min x y)

-- Question 5: maximum
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just (max x y)

-- Question 6: null
null' :: (Foldable t) => t a -> Bool
null' = foldr (\x y -> False) True
--null' t = getAll $ foldMap (const (All False)) t

-- Question 7: length
length' :: (Foldable t) => t a -> Int
--length' = foldr (\_ x -> x + 1) 0
length' f = getSum $ foldMap (const (Sum 1)) f

-- Question 8: toList
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- Question 9: fold
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- Question 10: foldMap
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Chapter Exercises (pages 819-820)
-- Question 1
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
  foldl f z (Constant x) = f z x
  foldMap f (Constant x) = f x

-- Question 2
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two _ x) = f x z
  foldl f z (Two _ x) = f z x
  foldMap f (Two _ x) = f x

-- Question 3
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z
  foldl f z (Three _ _ x) = f z x
  foldMap f (Three _ _ x) = f x

-- Question 4
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ _ x) = f x z
  foldl f z (Three' _ _ x) = f z x
  foldMap f (Three' _ _ x) = f x

-- Question 5
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ x) = f x z
  foldl f z (Four' _ _ _ x) = f z x
  foldMap f (Four' _ _ _ x) = f x

-- Question 6
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
