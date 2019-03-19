-- Chapter 11: Algebraic Data Types
-- Phone exercise (pages 454-457)
-- This version uses a binary search tree

import Data.Char
import Data.List

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- Binary search tree (BST): Key is Char, Value is [(Digit, Presses)]
data DaPhone = 
    Node Char [(Digit, Presses)] DaPhone DaPhone 
  | Leaf
  deriving (Eq, Show)

-- The binary search tree that describes the phone
phone :: DaPhone
phone = Node 'S' [('*', 1), ('7', 4)]
          (Node 'B' [('*', 1), ('2', 2)]
            (Node '2' [('2', 4)]
              (Node ',' [('#', 2)] 
                (Node '*' [('*', 2)] 
                  (Node '#' [('#', 3)]  
                    (Node ' ' [('0', 2)] Leaf Leaf) Leaf)
                  (Node '+' [('0', 1)] Leaf Leaf))
                (Node '0' [('0', 3)] 
                  (Node '.' [('#', 1)] Leaf Leaf)
                  (Node '1' [('1', 1)] Leaf Leaf)))
              (Node '7' [('7', 5)]
                (Node '5' [('*', 2)]
                  (Node '4' [('4', 4)] 
                    (Node '3' [('3', 4)] Leaf Leaf) Leaf)
                  (Node '6' [('6', 4)] Leaf Leaf))
                (Node '9' [('*', 2)] 
                  (Node '8' [('8', 4)] Leaf Leaf)
                  (Node 'A' [('*', 1), ('2', 1)] Leaf Leaf))))
            (Node 'K' [('*', 1), ('5', 2)]
              (Node 'G' [('*', 1), ('4', 1)] 
                (Node 'E' [('*', 1), ('3', 2)] 
                  (Node 'D' [('*', 1), ('3', 1)] 
                    (Node 'C' [('*', 1), ('2', 3)] Leaf Leaf) Leaf)
                  (Node 'F' [('*', 1), ('3', 3)] Leaf Leaf))
                (Node 'I' [('*', 1), ('4', 3)] 
                  (Node 'H' [('*', 1), ('4', 2)] Leaf Leaf)
                  (Node 'J' [('*', 1), ('5', 1)] Leaf Leaf)))
              (Node 'O' [('*', 1), ('6', 3)] 
                (Node 'M' [('*', 1), ('6', 1)] 
                  (Node 'L' [('*', 1), ('5', 3)] Leaf Leaf)
                  (Node 'N' [('*', 1), ('6', 2)] Leaf Leaf))
                (Node 'Q' [('*', 1), ('7', 2)] 
                  (Node 'P' [('*', 1), ('7', 1)] Leaf Leaf)
                  (Node 'R' [('*', 1), ('7', 3)] Leaf Leaf)))))
          (Node 'j' [('5', 1)] 
            (Node 'b' [('2', 2)]
              (Node 'X' [('*', 1), ('9', 2)] 
                (Node 'V' [('*', 1), ('8', 3)] 
                  (Node 'U' [('*', 1), ('8', 2)] 
                    (Node 'T' [('*', 1), ('8', 1)] Leaf Leaf) Leaf)
                  (Node 'W' [('*', 1), ('9', 1)] Leaf Leaf))
                (Node 'Z' [('*', 1), ('9', 4)] 
                  (Node 'Y' [('*', 1), ('9', 3)] Leaf Leaf)
                  (Node 'a' [('2', 1)] Leaf Leaf)))
              (Node 'f' [('3', 3)] 
                (Node 'd' [('3', 1)] 
                  (Node 'c' [('2', 3)] Leaf Leaf)
                  (Node 'e' [('3', 2)] Leaf Leaf))
                (Node 'h' [('4', 2)] 
                  (Node 'g' [('4', 1)] Leaf Leaf)
                  (Node 'i' [('4', 3)] Leaf Leaf))))
            (Node 's' [('7', 4)]
              (Node 'o' [('6', 3)]
                (Node 'm' [('6', 1)] 
                  (Node 'l' [('5', 3)] 
                    (Node 'k' [('5', 2)] Leaf Leaf) Leaf)
                  (Node 'n' [('6', 2)] Leaf Leaf))
                (Node 'q' [('7', 2)] 
                  (Node 'p' [('7', 1)] Leaf Leaf)
                  (Node 'r' [('7', 3)] Leaf Leaf)))
              (Node 'w' [('9', 1)] 
                (Node 'u' [('8', 2)] 
                  (Node 't' [('8', 1)] Leaf Leaf)
                  (Node 'v' [('8', 3)] Leaf Leaf))
                (Node 'y' [('9', 3)] 
                  (Node 'x' [('9', 2)] Leaf Leaf)
                  (Node 'z' [('9', 4)] Leaf Leaf)))))

-- Perform an in-order traversal of the phone BST
inOrder :: DaPhone -> [(Char, [(Digit, Presses)])]
inOrder Leaf = []
inOrder (Node key value left right) = 
  (inOrder left) ++ [(key, value)] ++ (inOrder right)

-- List of sample conversations
convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps Leaf _ = []
reverseTaps (Node key value left right) ch
  | ch < key  = reverseTaps left ch
  | ch == key = value
  | ch > key  = reverseTaps right ch

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = foldMap . reverseTaps

-- count total presses
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter = mostFrequent . filter isAlpha

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostFrequent . words . intercalate " " 

mostFrequent :: (Ord a) => [a] -> a
mostFrequent = head . longest . group . sort
  where longest = maximumBy (\x y -> compare (length x) (length y))

