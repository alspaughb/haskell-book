-- Chapter 05: Types
-- Exercises: Type Matching (pages 127-128)
-- Question 1: Functions
-- Question 1a: c) not :: Bool -> Bool
-- Question 1b: d) length :: [a] -> Int
-- Question 1c: b) concat :: [[a]] -> [a]
-- Question 1d: a) head :: [a] -> a
-- Question 1e: e) (<) :: Ord a => a -> a -> Bool

-- Exercises: Type Arguments (pages 136-139)
-- Question 1: a) Char -> Char -> Char
-- Question 2: d) Char
-- Question 3: d) Num b => b
-- Question 4: c) Double
-- Question 5: a) [Char]
-- Question 6: e) Eq b => b -> [Char]
-- Question 7: d) (Ord, Num a) => a
-- Question 8: a) (Num a, Ord a) => a
-- Question 9: c) Integer

-- Exercises: Parametricity (pages 141-142)
-- Question 1: No other implementation
-- Question 2:
myFirst :: a -> a -> a
myFirst = const

mySecond :: a -> a -> a
mySecond = flip $ const
-- Question 3:
bravo :: a -> b -> b
bravo = flip $ const

-- Exercises: Apply Yourself (pages 146-147)
-- Question 1: myConcat :: [Char] -> [Char]
-- Question 2: muMult :: Fractional a => a -> a
-- Question 3: myTake :: Int -> [Char]
-- Question 4: myCom :: Int -> Bool
-- Question 5: myAlph :: Char -> Bool

-- Chapter Exercises (pages 149-159)
-- Multiple Choice (pages 149-150)
-- Question 1: c) a list whose elements are all of some type a
-- Question 2: a) take a list of strings as an argument
-- Question 3: b) returns one element of type a from a list
-- Question 4: c) takes a tuple argument and returns the first value

-- Determine the type (pages 150-152)
-- Question 1a: 54 :: Num a => a
-- Question 1b: (0, "doge") :: Num a => (a, [Char])
-- Question 1c: (0, "doge") :: (Ingeter, [Char])
-- Question 1d: 5 :: Int
-- Question 1e: False :: Bool
-- Question 2: 50 :: Num a => a
-- Question 3: z :: Num a => a -> a
-- Question 4: 0.4 :: Fractional a => a
-- Question 5: "Julie <3 Haskell" :: [Char]

-- Does it compile?
-- Question 1: Does not compile
bigNum = (^) 5 $ 10
wahoo = bigNum + 10
-- Question 2: compiles
-- Question 3: Does not compile
-- a = 5
-- b = (+)
-- c = b 10
-- d = c 200
-- Question 4: c is undefined
-- a = 12 + b
-- b = 1000 * 12

-- Type variable or specific type constructor? (pages 152-153)
-- Question 1: [0]: Constrained polymorphic (Num)
--             [1]: Fully polymorphic
--             [2]: Concrete (Int)
--             [3]: Concrete (Int)
-- Question 2: [0]: Fully polymorphic
--             [1]: Concrete (Zed)
--             [2]: Concrete (Blah)
-- Question 3: [0]: Fully polymorphic
--             [1]: Constrained polymorphic (Enum)
--             [2]: Concrete (C)
-- Question 4: [0]: Fully polymorphic
--             [1]: Fully polymorphic
--             [2]: Concrete (C)

-- Write a type signature (page 153)
-- Question 1
functionH :: [a] -> a
functionH (x:_) = x
-- Question 2
functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False
-- Question 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function (page 153-155)
-- Question 1
i :: a -> a
i = id
-- Question 2
c :: a -> b -> a
c = const
-- Question 3:
c'' :: b -> a -> b
c'' = const
-- Question 4:
c' :: a -> b -> b
c' = flip $ const
-- Question 5:
r :: [a] -> [a]
r = reverse
-- r = tail
-- Question 6:
co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g
-- Question 7:
a :: (a -> c) -> a -> a
a = flip $ const
-- Question 8:
a' :: (a -> b) -> a -> b
a' f = f

-- Fix it (page 156)
-- Question 1 and 2: See Sing.hs
-- Question 3: See Arith3Broken.hs

-- Type-Kwon-Do (pages 156-159)
-- Question 1:
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g.f

-- Question 2:
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w.q
-- Question 3:
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)
-- Question 4
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge f g x = fst (g (f x))
