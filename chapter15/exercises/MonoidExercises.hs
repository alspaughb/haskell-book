-- Chapter 15: Monoid, Semigroup
-- Monoid exercises (pages 609-612)
import Test.QuickCheck
import Data.Monoid

-- Question 1: Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Question 2: Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Question 3: Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two w x) <> (Two y z) = Two (w <> y) (x <> z)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- Question 4: BoolConj
newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)
  
instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)
  
instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Question 5: BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Question 6: Combine
newtype Combine a b =
  Combine { unCombine :: a -> b}
  
instance Show (Combine a b) where
  show (Combine _) = "Combine { unCombine :: a -> b}"
  
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

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

combineLeftIdentity :: (Monoid b, Eq b) => Combine a b -> a -> Bool
combineLeftIdentity c x = unCombine (mempty <> c) x == unCombine c x

combineRightIdentity :: (Monoid b, Eq b) => Combine a b -> a -> Bool
combineRightIdentity c x = unCombine (c <> mempty) x == unCombine c x

-- Question 7: Comp
newtype Comp a =
  Comp { unComp :: a -> a}
  
instance Show (Comp a) where
  show _ = "Comp { unComp :: a -> a}"
  
instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

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

compLeftIdentity :: Eq a => Comp a -> a -> Bool
compLeftIdentity c x = unComp (mempty <> c) x == unComp c x

compRightIdentity :: Eq a => Comp a -> a -> Bool
compRightIdentity c x = unComp (c <> mempty) x == unComp c x

-- Question 8: Mem
newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Show (Mem s a) where
  show _ = "Mem { runMem :: s -> (a, s)}"

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem 
    (\s -> (fst (f s) <> fst (g s), snd (f (snd (g s)))))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Mem s a) where
  arbitrary = arbitrary >>= return . Mem

f' :: Num s => Mem s String
f' = Mem $ \s -> ("hi", s + 1)

memAssoc :: (Semigroup a, Eq s, Eq a) => 
  Mem s a -> Mem s a -> Mem s a -> s -> Bool
memAssoc f g h s = runMem (f <> (g <> h)) s == runMem ((f <> g) <> h) s

type MemAssoc s a 
    = Mem s a
    -> Mem s a
    -> Mem s a
    -> s
    -> Bool

memLeftIdentity :: (Monoid a, Eq s, Eq a) => Mem s a -> s -> Bool
memLeftIdentity f s = runMem (mempty <> f <> mempty) s == runMem f s

memRightIdentity :: (Monoid a, Eq s, Eq a) => Mem s a -> s -> Bool
memRightIdentity f s = runMem (f <> mempty) s == runMem f s

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  putStrLn "Question 1: Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "Question 2: Identity"
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  putStrLn "Question 3: Two"
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  putStrLn "Question 4: BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "Question 5: BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "Question 6: Combine"
  quickCheck (combineAssoc :: CombineAssoc Int (Sum Int))
  quickCheck (combineLeftIdentity :: Combine Int (Sum Int) -> Int -> Bool)
  quickCheck (combineRightIdentity :: Combine Int (Sum Int) -> Int -> Bool)

  putStrLn "Question 7: Comp"
  quickCheck (compAssoc :: CompAssoc Int)
  quickCheck (compLeftIdentity :: Comp Int -> Int -> Bool)
  quickCheck (compRightIdentity :: Comp Int -> Int -> Bool)

  putStrLn "Question 8: Mem"
  print $ runMem (f' <> mempty) (0 :: Int)
  print $ runMem (mempty <> f') (0 :: Int)
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' (0 :: Int)
  print $ runMem (mempty <> f') 0 == runMem f' (0 :: Int)

  quickCheck (memAssoc :: MemAssoc Int String)
  quickCheck (memLeftIdentity :: Mem Int String -> Int -> Bool)
  quickCheck (memRightIdentity :: Mem Int String -> Int -> Bool)