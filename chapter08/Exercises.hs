-- Chapter 8: Functions That Call Themselves
import Data.List (intersperse)

-- Intermission: Exercise (page 283)
applyTimes :: (Eq a, Num a) =>
              a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

-- applyTimes 5 (+1) 5
--   = (+1) . (applyTimes 4 (+1)) $ 5
--   = (+1) . (+1) . (applyTimes 3 (+1)) $ 5
--   = (+1) . (+1) . (+1) . (applyTimes 2 (+1)) $ 5
--   = (+1) . (+1) . (+1) . (+1) . (applyTimes 1 (+1)) $ 5
--   = (+1) . (+1) . (+1) . (+1) . (+1) . (applyTimes 0 (+1)) $ 5
--   = (+1) . (+1) . (+1) . (+1) . (+1) $ 5
--   = 10

-- Rewrite applyTimes to avoid stack overflow
applyTimes' :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes' 0 _ b = b
applyTimes' n f b = applyTimes' (n-1) f $! (f b) 

-- Chapter Exercises
-- Review of types (pages 294 - 295)
-- Question 1: d) [[Bool]]
-- Question 2: b) [[3 == 3], [6 > 5], [3 < 4]]
-- Question 3: d) all of the above
-- Question 4: b) func "Hello" "World"

-- Reviewing currying (pages 295 -296)
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

-- "woops mrow " s
appedCatty :: String -> String
appedCatty = cattyConny "woops"

-- s " mrow haha"
frappe :: String -> String
frappe = flippy "haha"

-- Question 1: appedCatty "woohoo!"
--   "woops mrow woohoo!"
-- Question 2: frappe "1"
--   "1 mrow haha"
-- Question 3: frappe (appedCatty "2")
--   "woops mrow 2 mrow haha"
-- Question 4: appedCatty (frappe "blue")
--   "woops mrow blue mrow haha"
-- Question 5: cattyConny (frappe "pink")
--                        (cattyConny "green" (appedCatty "blue"))
--  "pink mrow haha mrow green woops mrow blue"
-- Question 6: cattyConny (flippy "Pugs" "are") "awesome"
--  "are mrow Pugs mrow awesome"

-- Recursion (page 296)
-- Question 1
-- dividedBy 15 2
--   = go 15 2 0
--   = go 13 2 1
--   = go 11 2 2
--   = go 9 2 3
--   = go 7 2 4
--   = go 5 2 5
--   = go 3 2 6
--   = go 1 2 7
--   = (7, 1)
-- Question 2:
mySum :: (Eq a, Num a) => a -> a
mySum 1 = 1
mySum n = n + mySum (n-1)
-- Question 3:
myMultiply :: Integral a => a -> a -> a
myMultiply x y
  | y < 0          = -myMultiply x (-y)
  | y == 0         = 0
  | otherwise      = x + myMultiply x (y-1)
-- Question 4:
data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

myDividedBy :: Integral a => a -> a -> DividedResult
myDividedBy num denom
  | denom == 0           = DividedByZero
  | num < 0 && denom < 0 = Result (go (negate num) (negate denom) 0)
  | num < 0              = Result (negate (go (negate num) denom 0))
  | denom < 0            = Result (negate (go num (negate denom) 0))
  | otherwise            = Result (go num denom 0)
  where go n d count
          | n < d     = count
          | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function (page 297)
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 (mc91 (n + 11))

-- Numbers into words (page 298-299)
digitToWord :: Int -> String
digitToWord n = digitWords !! n
  where digitWords = ["zero", "one", "two", "three", "four"
                     , "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ fmap digitToWord (digits n)
