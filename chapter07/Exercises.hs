-- Chapter 7: More Functional Patterns

-- Exercises: Grab Bag (page 226)
-- Question 1: a, b, c, and d are all equivalent
-- Question 2: d) Num a => a -> a -> a
-- Question 3a:
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1
addOneIfOdd' = \n -> if odd n then n + 1 else n
-- Question 3b:
addFive = \x y -> (if x > y then y else x) + 5
-- Question 3c:
mflip f x y = f y x

-- Exercises: Variety Pack (page 237)
-- Question 1a: k :: (a, b) -> a
-- Question 1b: k2 :: [Char] (different type from k1 and k3)
-- Question 1c: k1, k3
-- Question 2:
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Exercises: Case Practice (page 239-240)
-- Question 1:
functionC x y = 
    case x > y of
      True -> x
      _    -> y
-- Question 2:
ifEvenAdd2 n = 
    case even n of
      True -> n + 2
      _    -> n
-- Question 3:
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Exercises: Artful Dodgy
dodgy x y = x + y * 10
oneIsOne = dodgy 1 -- 10 times the given value + 1
oneIsTwo = (flip dodgy) 2 -- the given value plus 20
-- Question 1: dodgy 1 0 ==> 1
-- Question 2: dodgy 1 1 ==> 11
-- Question 3: dodgy 2 2 ==> 22
-- Question 4: dodgy 1 2 ==> 21
-- Question 5: dodgy 2 1 ==> 12
-- Question 6: oneIsOne 1 ==> 11
-- Question 7: oneIsOne 2 ==> 21
-- Question 8: oneIsTwo 1 ==> 21
-- Question 9: oneIsOne 3 ==> 31
-- Question 10: oneIsTwo 3 ==> 23

-- Exercises: Guard Duty (pages 253-254)
-- Question 1:
avgGrade1 :: (Fractional a, Ord a) => a -> Char
avgGrade1 x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100
-- Answer: Always outputs 'F'
-- Question 2:
avgGrade2 :: (Fractional a, Ord a) => a -> Char
avgGrade2 x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100
-- Answer: Anything 70 or greater outputs 'C', 
--   everything else is 'D' or 'F'
-- Question 3: b) True when xs is a plaindrome
pal xs
    | xs == reverse xs = True
    | otherwise        = False
-- Question 4: Eq a => [a]
-- Question 5: pal :: Eq a => [a] -> Bool
-- Question 6:
numbers x
    | x < 0   = -1
    | x == 0  = 0
    | x > 0   = 1
-- Answer: c) an indication of whether its argument is a positive
--   or negative number or zero
-- Question 7: (Num a, Ord a) => a
-- Question 8: numbers:: (Num a, Ord a, Num b) => a -> b

-- Chapter Exercises
-- Multiple Choice
-- Question 1: d) may resolve to values of different types,
--   depending on inputs
-- Question 2: b) Char -> [String]
-- Question 3: d) (Ord a, Num a) => a -> Bool
-- Question 4: b) is a higher-order function
-- Question 5: a) f True :: Bool

-- Let's write code
-- Question 1a, 1b:
tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10
-- Question 1c:
hunsD :: Integral a => a -> a
hunsD x = d2
  where d = x `div` 100
        d2 = d `mod` 10
-- Question 2;
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z =
  case z of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z         = y
  | otherwise = x

-- Question 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


-- Question 4, 5, 6: See arith4.hs
