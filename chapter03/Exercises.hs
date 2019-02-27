-- Chapter 3: Simple Operations With Text

-- Exercises: Scope (page 74)
-- Question 1: In scope
-- Question 2: Not in scope (h is not defined)
-- Question 3: Not in scope (missing where clause for r)
-- Question 4: In scope

-- Exercises: Syntax Errors (page 77)
-- Question 1: Needs fix
--   (++) [1, 2, 3] [4, 5, 6]
-- Question 2: Needs fix
--   "<3" ++ " Haskell"
-- Question 3: No fix needed
--   concat ["<3", " Haskell"]

-- Chapter Exercises (pages 82-86)

-- Reading syntax (pages 82-83)
--   Question 1a: Correct: [1, 2, 3, 4, 5, 6]
--   Question 1b: Correction: (++) [1, 2, 3] [4, 5, 6]
--   Question 1c: Correct: "hello world"
--   Question 1d: Correction: ["hello" ++ " world"]
--   Question 1e: Correction "hello" !! 4
--   Question 1f: Correct: 'o'
--   Question 1g: Correction: take 4 "lovely"
--   Question 1h: Correct: "awe"

--   Question 2a: d [6, 12, 18]
--   Question 2b: c "rainbow"
--   Question 2c: e 10
--   Question 2d: a "Jules"
--   Question 2e: b [2, 3, 5, 6, 8, 9]

-- Building functions (pages 83-86)
--   Question 1a: "Curry is awesome" ++ "!"
--   Question 1b: "Curry is awesome!" !! 4
--   Question 1c: drop 9 "Curry is awesome!"

--   Question 2a
appendExclaimationPoint :: String -> String
appendExclaimationPoint = (++ "!")

--   Question 2b
fourthLetter :: String -> String
fourthLetter s = [s !! 4]

--   Question 2c
lastWord :: String -> String
lastWord = last.words

--   Question 3
thirdLetter :: String -> Char
thirdLetter = (!! 2)

--   Question 4
letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i

--   Question 5
rsrv :: String -> String
rsrv s =
  awesome ++ " " ++ is ++ " " ++ curry
  where curry = take 5 s
        d1 = drop 6 s
        is = take 2 d1
        d2 = drop 3 d1
        awesome = take 7 d2

-- Question 6: See Reverse.hs
