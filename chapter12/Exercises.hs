-- Chaper 12: Signaling Adversity

import Data.List (intercalate)
import Data.Char (toLower)

-- Chapter Exercises
-- Determine the kinds (page 477)
-- Question 1: a is a type argument, not a type constructor
--             and therefore does not have a kind
-- Question 2: a and f are type arguments, not type constructors,
--             and therefore do not have kinds

-- String processing (pages 478-479)
-- Question 1
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe word  = Just word

replaceThe :: String -> String
replaceThe s = intercalate " " $ map f $ words s
  where f word = g (notThe word)
        g Nothing     = "a"
        g (Just word) = word

-- Simplify by using the maybe function
replaceThe' :: String -> String
replaceThe' s = intercalate " " $ map f $ words s
  where f w = maybe "a" id (notThe w)

-- Question 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 (words str)
  where go :: Integer -> [String] -> Integer
        go c []         = c
        go c (w:[])     = c
        go c (w1:w2:ws) = if w1 == "the" && vowelInitial w2
                             then go (c+1) (w2:ws)
                             else go c (w2:ws)
        vowelInitial ""    = False
        vowelInitial (x:_) = elem x "aeiouAEIOU"

-- Question 3
countVowels :: String -> Integer
countVowels word = fromIntegral $ length $ filter isVowel word
  where isVowel x = elem x "aeiouAEIOU"

-- Use foldr to avoid creating a list
countVowels' :: String -> Integer
countVowels' = foldr (\x c -> if elem x "aeiouAEIOU" then c+1 else c) 0

-- Validate the word (page 479)
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if consonantCnt >= vowelCnt
           then Just (Word' w)
           else Nothing
  where isVowel c    = elem (toLower c) vowels
        vowelCnt     = length (filter isVowel w)
        consonantCnt = length w - vowelCnt

-- It's only Natural (page 479)
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | i == 0    = Just Zero
  | otherwise = fmap Succ (integerToNat (i-1))

-- Use a go function instead of fmap
integerToNat' :: Integer -> Maybe Nat
integerToNat' i = if i < 0 then Nothing else Just (go i)
  where go 0 = Zero
        go i = Succ (go (i - 1))

-- Small library for Maybe (pages 480-482)
-- Question 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not.isJust

-- Question 2:
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee y _ Nothing  = y

-- Question 3
fromMaybe :: a -> Maybe a -> a
fromMaybe x m = mayybee x id m

-- Question 4
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listTomaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- Question 5
catMaybes :: [Maybe a] -> [a]
catMaybes ms = foldr f [] ms
  where f Nothing  xs = xs
        f (Just x) xs = x : xs

-- Question 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = foldr f (Just []) ms
  where f Nothing  _         = Nothing
        f _        Nothing   = Nothing
        f (Just x) (Just xs) = Just (x:xs)

-- Small library for Either
-- Question 1
lefts' :: [Either a b] -> [a]
lefts' es = foldr f [] es
  where f (Left x) xs = x : xs
        f _        xs = xs
        
-- Question 2
rights' :: [Either a b] -> [b]
rights' es = foldr f [] es
  where f (Right x) xs = x : xs
        f _         xs = xs

-- Question 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = foldr f ([], []) es
  where f (Left x)  (xs, ys) = (x:xs, ys)
        f (Right y) (xs, ys) = (xs, y:ys)

-- Question 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' f _         = Nothing

-- Question 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

-- Question 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e =
  either' (\a -> Nothing) (\b -> Just (f b)) e

-- Write your own iterate and unfoldr (pags 485-486)
-- Question 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- Question 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f y = g (f y)
  where g Nothing       = []
        g (Just (x, y)) = x : myUnfoldr f y

-- Question 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x

-- Finally something other than a list! (pages 486-487)
-- Question 1
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Question 1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = go (f x)
  where go Nothing                     = Leaf
        go (Just (left, value, right)) =
          Node (go (f left)) value (go (f right))

-- Question 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f h = if (h < n) then Just (h+1, h, h+1) else Nothing
