-- Chapter 9: Lists
import Data.Bool
import Data.Char

-- Exercise: EnumFromTo (page 307)
eftBool :: Bool -> Bool -> [Bool]
eftBool = eft
                
eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- My own enumFromTo definition
eft :: (Ord a, Enum a) => a -> a -> [a]
eft from to
  | from > to    = []
  | from == from = [from]
  | otherwise    = from : eft (succ from) to
  
-- Exercises: Thy Fearful Symmetry
-- Question 1: myWords
myWords :: String -> [String]
myWords str = mySplit str ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines str = mySplit str '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)
  
-- Question 3:
mySplit :: String -> Char -> [String]
mySplit "" _   = []
mySplit str ch = takeWhile (/= ch) str :
  (mySplit (dropWhile (== ch) (dropWhile (/= ch) str)) ch)

-- Exercises: Comprehend Thy Lists (page 315)
mySqr = [x^2 | x <- [1..10]]
-- mySqr = [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

list1 = [x | x <- mySqr, rem x 2 == 0]
-- list1 = [4, 16, 36, 64, 100]

list2 = [(x, y) | x <- mySqr,
                  y <- mySqr,
                  x < 50, y > 50]
-- list2 = [ (1, 64), (1, 81), (1, 100),
--         , (4, 64), (4, 81), (4, 100),
--         , (9, 64), (9, 81), (9, 100),
--         , (16, 64), (16, 81), (16, 100),
--         , (25, 64), (25, 81), (25, 100),
--         , (36, 64), (36, 81), (36, 100),
--         , (49, 64), (49, 81), (49, 100)]

list3 = take 5 [ (x, y) | x <- mySqr,
                          y <- mySqr,
                          x < 50, y > 50 ]
-- list3 = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]

-- Exercises: Square Cube (page 317)
mySqr'  = [x^2 | x <- [1..5]]
myCube' = [y^3 | y <- [1..5]]

-- Question 1:
tuples = [(x, y) | x <- mySqr', y <- myCube']

-- Question 2:
tuples' = [(x, y) | x <- mySqr', y <- myCube', x < 50, y < 50]

-- Question 3:
tupleCount = length tuples'

-- Exercises: Bottom Madness
-- Will it blow up? (page 326)
-- Question 1:
expr1 = [x^y | x <- [1..5], y <- [2, undefined]]
-- Result: ⊥

-- Question 2:
expr2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- Result: [1]

-- Question 3:
expr3 = sum [1, undefined, 3]
-- Result: ⊥

-- Question 4:
expr4 = length [1, 2, undefined]
-- Result: 3

-- Question 5:
expr5 = length $ [1, 2, 3] ++ undefined
-- Result: ⊥

-- Question 6:
expr6 = take 1 $ filter even [1, 2, 3, undefined]
-- Result: [2]

-- Question 7:
expr7 = take 1 $ filter even [1, 3, undefined]
-- Result: ⊥

-- Question 8:
expr8 = take 1 $ filter odd [1, 3, undefined]
-- Result: [1]

-- Question 9:
expr9 = take 2 $ filter odd [1, 3, undefined]
-- Result: [1, 3]

-- Question 10:
expr10 = take 3 $ filter odd [1, 3, undefined]
-- Result: ⊥

-- Intermission: Is it normal form? (page 326-327)
-- Question 1: 1 NF
-- Question 2: 2 WHNF
-- Question 3: 3 Neither
-- Question 4: 3 Neither
-- Question 5: 3 Neither
-- Question 6: 3 Neither
-- Question 7: 2 WHNF

-- Exercises: More Bottoms (333 - 334)
-- Question 1
q1 = take 1 $ map (+1) [undefined, 2, 3]
-- Result: ⊥

-- Question 2
q2 = take 1 $ map (+1) [1, undefined, 3]
-- Result: [2]

-- Question 3
q3 = take 1 $ map (+1) [1, undefined, 3]
-- Result: ⊥

-- Question 4
itIsMystery xs =
  map (\x -> elem x "aeiou") xs
-- Maps a list of Chars to a list of Bools based on
-- whether the corresponding Char is a lower case vowel

-- Question 5a
q5a = map (^2) [1..10]
-- Result: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- Question 5b
q5b = map minimum [[1..10], [10..20], [20..30]]
-- Result: [1, 10, 20]

-- Question 5c
q5c = map sum [[1..5], [1..5], [1..5]]
-- Result: [15, 15, 15]

-- Question 6
negate3 = fmap (\x -> bool (x) (-x) (x == 3)) [1..10]
-- Result: [1, 2, -3, 4, 5, 6, 7, 8, 9, 10]

-- Exercises: Filtering
-- Question 1
multiplesOfThree = filter (\x -> x `mod` 3 == 0) 

-- Question 2
multiplesOfThreeCount = (length . multiplesOfThree) [1..30]

-- Question 3
filterArticles sentence = filter f $ words sentence
  where f word = not $ elem word ["the", "a", "an"]

-- Zipping exercises (page 338)
-- Question 1
myZip :: [a] -> [b] -> [(a, b)]
myZip []     _      = []
myZip _      []     = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- Question 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []     _      = []
myZipWith _  _     []     = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

-- Question 3
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,) 

-- Chapter Exercises
-- Data.Char (pages 338-339)
-- Question 1
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- Question 2
onlyUpper = filter isUpper

-- Question 3
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter []     = []
capitalizeFirstLetter (x:xs) = toUpper x : xs

-- Question 4
capitalizeAll :: String -> String
capitalizeAll []     = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

-- Question 5
firstLetterCapitalized :: String -> Char
firstLetterCapitalized = toUpper . head

-- Ciphers (pages 339 - 341)
caesar :: Int -> String -> String
caesar offset = map shiftLetter
  where shiftLetter c
          | isUpper c = chr (mod (ord c - ord 'A' + offset) 26 + ord 'A')
          | isLower c = chr (mod (ord c - ord 'a' + offset) 26 + ord 'a')
          | otherwise = c

unCaesar :: Int -> String -> String
unCaesar offset = caesar (-offset)

-- Writing your own standard functions (pages 341-344)
-- Question 1
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = (f x) || myAny f xs

-- Question 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = e == x || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = myAny (==e) xs

-- Question 4
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Question 5
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- Question 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

-- Question 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 8
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) =
  if (f x y) == GT then x else y
  where y = myMaximumBy f xs

-- Question 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)

-- Question 10
myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs
