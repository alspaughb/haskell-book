-- Chapter 11: Algebraic Datatypes
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List (intercalate)

-- Exercises: Dog Types (pages 396-397)
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Question 1: Type constructor
-- Question 2: DogueDeBordeaux :: * -> *
-- Question 3: DogueDeBordeaux String :: *
-- Question 4: Husky 10 :: Num a => Husky a
-- Question 5: Husky (10 :: Integer) :: Husky Integer
-- Question 6: Mastiff "Scooby Doo" :: Mastiff [Char]
-- Question 7: Both a type constructor and a data constructor
-- Question 8: DogueDeBordeaux :: doge -> DogueDeBordeaux
-- Question 9: DogueDeBoreduaux "doggie!" :: DogueDeBordeaux [Char]

-- Exercises: Vehicles (page 399-400)
data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
               Mini
             | Mazda
             | Tata
               deriving (Eq, Show)

data Airline =
         PapuAir
       | CatapultsR'Us
       | TakeYourChancesUnited
         deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

-- Question 1: myCar :: Vehicle
-- Question 2:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- Question 3:
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu 

-- Question 4: Throws non-exhaustive patterns exception
-- Question 5:
data Vehicle' = Car' Manufacturer Price
              | Plane' Airline Integer
              deriving (Eq, Show)

isCar' :: Vehicle' -> Bool
isCar' (Car' _ _) = True
isCar' _          = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _            = False

areCars' :: [Vehicle'] -> [Bool]
areCars' = map isCar'

doge' = Plane' PapuAir 100

-- Exercises: Cardinality (pages 404-405)
-- Question 1: 1
-- Question 2: 3
-- Question 3: 2^16 = 65536
-- Question 4: Cardinality of Int = 2^64 = 1.8447E19
--             Cardinality of Integer is undefined
-- Question 5: 2^8 = 256

-- Exercises: For Example (pages 405-406)
data Example = MakeExample deriving Show

-- Question 1: MakeExample :: Example
--             Example is a type constructor not a data constructor,
--             so asking for its type will throw an exception
-- Question 2: Example has an instance of Show
-- Question 3:
data Example' = MakeExample' Int deriving Show
-- MakeExample' :: Int -> Example'

-- Exercise: Logic Goats (pages 410-411)
class TooMany a where
  tooMany :: a -> Bool

-- Question 1
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- Question 2
instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

-- Question 3
instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (x, y) = x + y > 42

-- Exercises: Pity the Bool (page 412-413)
-- Question 1: cardinality = 4
-- Question 2: cardinality = 256 + 2 = 258

-- Exercises: How Does Your Grow? (page 420)
-- Question 1:
type Gardener = String
data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show

-- Exercise: Programmers (430-431)
data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindJustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgLang =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang |
                   os <- allOperatingSystems
                 , lang <- allLanguages]

-- Exponentiation in what order? (page 439)
data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

--  2^3 = 8 implementations
convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = True
convert8 Both = True

-- Exercises: The Quad (pages 439-440)
-- Question 1: 4 + 4   = 8
-- Question 2: 4 * 4   = 16
-- Question 3: 4 ^ 4   = 256
-- Question 4: 2^3     = 8
-- Question 5: 2^(2*2) = 16
-- Question 6: 4^(2*4) = 65536

-- Write map for Binary Tree (pages 447-448)
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)
mapTree _ Leaf                = Leaf
  
testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- Convert binary trees to lists (449-450)
preorder :: BinaryTree a -> [a]
preorder (Node left a right) =
  [a] ++ (preorder left) ++ (preorder right)
preorder Leaf = []

inorder :: BinaryTree a -> [a]
inorder (Node left a right) =
  (inorder left) ++ [a] ++ (inorder right)
inorder Leaf = []

postorder :: BinaryTree a -> [a]
postorder (Node left a right) =
  (postorder left) ++ (postorder right) ++ [a]
postorder Leaf = []

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree (page 449)
-- inorder traversal
foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree f xs (Node left x right) =
  foldTree f (f x (foldTree f xs right)) left
foldTree _ xs Leaf = xs

-- Chapter Exercises
-- Multiple choice (pages 450-451)
-- Question 1: a) Weekday is a type with five data constructors
-- Question 2: c) f :: Weekday -> String
-- Question 3: b) must begin with a capital letter
-- Question 4: c) delivers the final element of xs

-- Ciphers (page 451-452)
type KeyText     = String
type MessageText = String
type CypherText  = String

vigenere :: KeyText -> MessageText -> CypherText
vigenere ks ms = zipWith f (cycle ks) ms
  where f k m  = shiftLetter (offset k) m

unVigenere :: KeyText -> CypherText -> MessageText
unVigenere ks cs = zipWith f (cycle ks) cs
  where f k c = shiftLetter (-offset k) c

shiftLetter :: Int -> Char -> Char
shiftLetter offset ch
  | isUpper ch = chr (mod (ord ch - ord 'A' + offset) 26 + ord 'A')
  | isLower ch = chr (mod (ord ch - ord 'a' + offset) 26 + ord 'a')
  | otherwise  = ch

offset :: Char -> Int
offset ch
  | isUpper ch = ord ch - ord 'A'
  | isLower ch = ord ch - ord 'a'
  | otherwise  = 0

-- As-patterns (pages 452-453)
-- Question 1: isSubseqOf
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf []         _      = True
isSubseqOf _          []     = False
isSubseqOf key@(k:ks) (x:xs) =
  if (k == x) then isSubseqOf ks xs
              else isSubseqOf key xs

-- Question 2: capitalizeWords
capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map f (words sentence)
  where f word@(c:cs) = (word, toUpper c : cs)

-- Language exercises (page 454)
-- Question 1: capitalizeWord
capitalizeWord :: String -> String
capitalizeWord []     = ""
capitalizeWord (c:cs) = toUpper c : cs

-- Question 2: capitalizeParagraph
-- recursive version
capitalizeParagraph :: String -> String
capitalizeParagraph para = intercalate " " (f (words para) True)
  where f :: [String] -> Bool -> [String]
        f []           _       = []
        f (word:words) capNext =
           (if capNext then capitalizeWord word else word)
           : f words (elem '.' word)

-- foldl version
capParagraph :: String -> String
capParagraph para = intercalate " " $ snd $ foldl f (True, []) (words para)
  where f (capNext, words) word = ((last word) == '.',
           if capNext then words ++ [capitalizeWord word]
                      else words ++ [word])

-- Phone exercise (pages 454-457)
-- See: Phone1.hs and Phone2.hs

-- Hutton's Razor
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a)           = a
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit a)           = show a
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
