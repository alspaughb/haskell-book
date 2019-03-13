-- Chapter 10: Folding lists
import Data.Time
import Data.Bool

-- Exercises: Understanding Folds
-- Question 1: b) and c)
-- Question 2:
-- let f = flip (*)
-- foldl f 1 [1..3]
--   = (((1 `f` 1) `f` 2) `f` 3)
--   = ((1 `f` 2) `f` 3)
--   = (2 `f` 3)
--   = 6
-- or
-- foldl (flip (*)) 1 [1..3]
--  = foldl (flip (*)) (flip (*) 1 1) [2..3]
--  = foldl (flip (*)) (flip (*) (flip (*) 1 1) 2) [3]
--  = foldl (flip (*)) (flip (*) (flip (*) (flip (*) 1 1) 2) 3) []
--  = flip (*) (flip (*) (flip (*) 1 1) 2) 3
--  = flip (*) (flip (*) 1 2) 3
--  = flip (*) 2 3
--  = 6
-- Question 3: c) foldr, but not foldl, associates to the right
-- Question 4: c) reduce structure
-- Question 5:
fold5a = foldr (++) "" ["woot", "WOOT", "woot"]
fold5b = foldr max 'a' "fear is the little death"
fold5c = foldr (&&) True [False, True]
fold5d = foldr (||) False [False, True]
fold5e = foldr ((++) . show) "" [1..5]
fold5f = foldl const 'a' [1..5]
fold5g = foldl const 0 "tacos"
fold5h = foldr (flip const) 0 "burritos"
fold5i = foldr (flip const) 'z' [1..5]

-- Exercises: Database Processing
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- Question 1:
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate t) ts = t : ts
        f _          ts = ts

-- Question 2:
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber n) ns = n : ns
        f _            ns = ns

-- Question 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- Question 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- Question 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = if count > 0 then total / count else 0
  where values = filterDbNumber xs
        total  = fromIntegral (sum values)
        count  = fromIntegral (length values)

-- Scans Exercises (page 379)
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Question 1:
fibs20 = take 20 fibs

-- Question 2:
smallFibs = takeWhile (<100) fibs

-- Question 3:
factorial = scanl (*) 1 [1..]
factorialN x = factorial !! x
factorial10 = take 10 factorial

-- Chapter Exercises
stops = "pbtdkg"
vowels = "aeiou"

-- Question 1a
tuples :: [a] -> [b] -> [c] -> [(a, b, c)]
tuples xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

-- Question 1b
pTuples :: [Char] -> [b] -> [c] -> [(Char, b, c)]
pTuples xs ys zs = filter (\(x, _, _) -> x == 'p') $ tuples xs ys zs

-- Question 1c
-- Answer to 1a would work for any list type, including nouns and verbs

-- Question 2: Function gives the average word length in the given string
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- Question 3: Rewrite question 2 using fractional division
avgWordLength s = numerator / denominator
  where wordLengths = map length (words s)
        numerator = fromIntegral (sum wordLengths)
        denominator = fromIntegral (length wordLengths)

-- Rewriting functions using folds (page 380)
-- Question 1:
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- Question 2:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- Question 3:
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (==e)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (==e)

-- Question 4:
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Question 5:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- Question 6:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> bool b (a : b) (f a)) []

-- Question 7:
squish :: [[a]] -> [a]
squish = foldr (++) []

-- Question 8:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- Question 9:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 10:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) =
  foldr (\a b -> bool b a ((f a b) == GT)) x xs

-- Question 11:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
