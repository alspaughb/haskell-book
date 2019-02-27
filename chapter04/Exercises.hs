-- Chapter 4: Because Pigs Can't Fly

-- Exercises: Mood Swing (page 90)
-- Question 1: Mood
-- Question 2: Blah and Woot
-- Question 3: changeMood :: Mood -> Mood
-- Question 4 and 5:

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

-- Exercises: Find the Mistakes (pages 103-104)
-- Question 1: not True && True
-- Question 2: not (x == 6)
-- Question 3: (1 * 2) > 5
-- Question 4: "Merry" > "Happy"
-- Question 5: ['1', '2', '3'] ++ "look at me!"

-- Chapter 4 Exercises (pages 111-115)
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- Question 1: length :: Foldable t => t a -> Int
--             length takes one Foldable argument and returns an Int

-- Question 2a: length [1, 2, 3, 4, 5] --> 5
-- Question 2b: length [(1, 2), (2, 3), (3, 4)] --> 3
-- Question 2c: length allAwesome --> 2
-- Question 2d: length (contact allAwesome) --> 5

-- Question 3: 6 / length [1, 2, 3]
--             The (/) operator is for Fractional numbers, and the Int 
--             returned by the length function is not Fractional
-- Question 4: 6 `div` length [1, 2, 3]
-- Question 5: Type is Bool, result is True
-- Question 6: Type is Bool, result is False

-- Question 7-1: True
-- Question 7-2: Won't work because elements are not all the same type
-- Question 7-3: 5
-- Question 7-4: False
-- Question 7-5: Won't work because the second parameter to the && operator is not a Bool

-- Question 8:
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = reverse x == x

-- Question 9:
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else negate x

-- Question 10:
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
--f (a, b) (c, d) = ((b, d), (a, c))

-- Correcting Syntax (pages 113-114)
-- Question 1: f xs = w `x` 1 where w = length xs
-- Question 2: \x -> x
-- Question 3: f (a, b) = a

-- Match the function names to their types
-- Question 1: c
-- Question 2: b
-- Question 3: a
-- Question 4: d




