-- Chapter 6: Less Ad-hoc Polymorphism
-- Exercises: Eq Instances (page 180)
-- Question 1
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

-- Question 2
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

-- Question 3
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x   == TisAnInt y   = x == y
  TisAString x == TisAString y = x == y
  _            == _            = False

-- Question 4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

-- Question 5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

-- Question 6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y
  _         == _         = False

-- Question 7
data EitherOr a b =
    Hello a
  | Goodbye a

instance Eq a => Eq (EitherOr a b) where
  Hello x   == Hello y   = x == y
  Goodbye x == Goodbye y = x == y
  _         == _         = False

-- Exercises: Tuple Experiment (page 182)
divMod :: Integral a => a -> a -> (a, a)
divMod x y = (x `div` y, x `mod` y)

quotRem :: Integral a => a -> a -> (a, a)
quotRem x y = (x `quot` y, x `rem` y)

-- Put on your thinking cap (page 184)
-- Fractional is a subset of Num
-- Num is a superclass of Fractional

-- Exercises: Will They work? (page 194)
-- Question 1: 5
-- Question 2: LT
-- Question 3: Type mismatch
-- Question 4: False

-- Chapter Exercises
-- Multiple Choice (pages 207-208)
-- Question 1: c) makes equality tests possible
-- Question 2: b) is a subclass of Eq
-- Question 3: a) Ord a => a -> a -> Bool
-- Question 4: c) the type of x is a tuple
-- Question 5: a) Int and Integer numbers

-- Does it typecheck? (pages 208-209)
-- Question 1: Person does not have an instance of Show
-- Question 2: Mood does not have an instance of Eq
-- Question 3: Does not typecheck (Mood does not have an Eq instance)
-- Question 4: typechecks

-- Given a datatype declaration, what can we do? (page 210)
-- Question 1: No -- data constructors of Rocks and Yeah not specified
-- Question 2: Yes
-- Question 3: Yes
-- Question 4: No -- No instance of Ord for Papu, Rocks or Yeah

-- Match the types (page 211-212)
-- Question 1: No instance of Num
-- Question 2: Can't deduce (Fractional a) arising from the literal '1.0'
-- Question 3: Yes
-- Question 4: Yes
-- Question 5: Yes
-- Question 6: Yes
-- Question 7: Yes
-- Question 8: Couldn't match type 'a' with 'Int'
-- Question 9: Yes, if you import sort
-- Question 10: Yes, if you import sort
-- Question 11: Couldn't match type 'a' with 'Char'

-- Type-Kwon-Do Two: Electric Typealoo (pages 212-213)
-- Question 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

-- Question 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (f y) + fromInteger x
