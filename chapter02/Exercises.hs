-- Chapter 02: Basic Expressions and Functions

-- Exercises: Comprehension Check (page 35)
-- Question 1
half x = x / 2
square x = x * x

-- Question 2
area1 r = 3.14 * square r

-- Question 3
area2 r = pi * square r

-- Exercises: Parenthesis and Association (page 39)
-- Question 1
--   Parentheses change the results of the functions
-- Question 2
--   Parentheses do not change the results of the functions
-- Question 3
--   Parentheses change the results of the functions

-- Exercises: Heal the Sick (pages 45-46)
-- Question 1
area x = 3.14 * (x * x)

-- Question 2
double x = x * 2

-- Question 3
x = 7
y = 10
f = x + y

-- Exercises: A Head Code (page 59)
-- Question 1: let x = 5 in x
--   5
-- Question 2: let x = 5 in x * x
--   25
-- Question 3: let x = 5; y = 6 in x * y
--   30
-- Question 4: let x = 3; y = 1000 in x + 3
--   6

-- Exercises: A Head Code continued (page 60)
-- Rewrite the functions with where clauses
-- Question 1: let x = 3; y = 1000 in x * 3 + 7
q1 = x * 3 + y
  where x = 3
        y = 1000

-- Question 2: let y = 10; x = 10 * 5 + y in x * 5
q2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- Question 3:
q3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- Chapter Exercises (pages 60-63)
-- Parenthesization
--   Question 1: 2 + (2 * 3) - 1
--   Question 2: 10 ^ (1 + 1)
--   Question 3: ((2 ^ 2) * (4 ^ 5)) + 1
-- Equivalent expressions
--   Question 1: Same result (2)
--   Question 2: Same result (100)
--   Question 3: Different results: 363 and -363 respectively
--   Question 4: Different results: 33 and 33.333333333333336 respectively
--   Question 5: Different results: 28 and 46 respectively
-- More fun with functions
--   Question 1a: 1135
--   Question 1b: 1135
--   Question 1c: -1110
--   Question 1d: 1110
--   Question 3: 3375
--   Question 4: Rewrite waxOn using where
waxOn = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8
-- Question 5: triple
triple x = x * 3
-- Question 6: waxOff
waxOff x = triple x

