-- Chapter 15: Monoid, Semigroup
-- Semigroup exercises (pages 606-609)
import Test.QuickCheck (Arbitrary, CoArbitrary, quickCheck, arbitrary, elements)
import Data.Monoid

-- Question 1: Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Question 2: Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Question 3: Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two w x) <> (Two y z) = Two (w <> y) (x <> z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- Question 4: Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => 
    Semigroup (Three a b c) where
  (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
    Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- Question 5: Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
  (Four a b c d) <> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- Question 6: BoolConj
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Question 7: BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Question 8: Or
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> (Snd y) = Snd y
  (Fst _) <> (Fst y) = Fst y
  (Snd x) <> _       = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- Question 9: Combine
newtype Combine a b =
  Combine { unCombine :: a -> b}

instance Show (Combine a b) where
  show (Combine _) = "Combine { unCombine :: a -> b}"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = arbitrary >>= return . Combine

combineAssoc :: (Semigroup b, Eq b) => 
  Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc f g h x =
  unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

type CombineAssoc a b
  = Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool

-- Question 10: Comp
newtype Comp a =
  Comp { unComp :: a -> a}

instance Show (Comp a) where
  show _ = "Comp { unComp :: a -> a}"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = arbitrary >>= return . Comp
 
compAssoc :: Eq a => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

type CompAssoc a 
  = Comp a
  -> Comp a
  -> Comp a
  -> a
  -> Bool

-- Question 11: Validation
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success x) <> (Failure _) = Success x
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Success x) <> (Success _) = Success x
  (Failure _) <> (Success y) = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Failure x, Success y]

type ValidationAssoc a b 
  = Validation a b
  -> Validation a b
  -> Validation a b
  -> Bool

-- Question 12: AccumulateRight
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success x) <> AccumulateRight (Success y) = AccumulateRight (Success (x <> y))
  AccumulateRight (Failure x) <> _ = AccumulateRight (Failure x)
  _ <> AccumulateRight (Failure y) = AccumulateRight (Failure y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [AccumulateRight (Failure x), AccumulateRight (Success y)]

type AccumulateRightAssoc a b 
  = AccumulateRight a b
  -> AccumulateRight a b
  -> AccumulateRight a b
  -> Bool

-- Question 13: AccumulateBoth
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success x) <> AccumulateBoth (Success y) = AccumulateBoth (Success (x <> y))
  AccumulateBoth (Failure x) <> AccumulateBoth (Failure y) = AccumulateBoth (Failure (x <> y))
  AccumulateBoth (Failure x) <> _ = AccumulateBoth (Failure x)
  _ <> AccumulateBoth (Failure y) = AccumulateBoth (Failure y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [AccumulateBoth (Failure x), AccumulateBoth (Success y)]

type AccumulateBothAssoc a b 
  = AccumulateBoth a b
  -> AccumulateBoth a b
  -> AccumulateBoth a b
  -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  putStrLn "Question 1: Trivial"
  quickCheck (semigroupAssoc :: TrivialAssoc)

  putStrLn "Question 2: Identity"
  quickCheck (semigroupAssoc :: IdentityAssoc String)

  putStrLn "Question 3: Two"
  quickCheck (semigroupAssoc :: TwoAssoc String String)

  putStrLn "Question 4: Three"
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)

  putStrLn "Question 5: Four"
  quickCheck (semigroupAssoc :: FourAssoc String String String String)

  putStrLn "Question 6: BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  
  putStrLn "Question 7: BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  
  putStrLn "Question 8: Or"
  quickCheck (semigroupAssoc :: OrAssoc Int Int)
  
  putStrLn "Question 9: Combine"
  quickCheck (combineAssoc :: CombineAssoc Int (Sum Int))
  
  putStrLn "Question 10: Comp"
  quickCheck (compAssoc :: CompAssoc (Sum Int))
  
  putStrLn "Question 11: Validation"
  let failure :: String
              -> Validation String Int
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

  quickCheck (semigroupAssoc :: ValidationAssoc [String] Int)
  
  putStrLn "Question 12: AccumulateRight"
  quickCheck (semigroupAssoc :: AccumulateRightAssoc Int [String])
  
  putStrLn "Question 13: AccumulateBoth"
  quickCheck (semigroupAssoc :: AccumulateBothAssoc [String] [String])