-- Chapter 14: Testing
-- Using QuickCheck (pages 562-563)

import Test.QuickCheck
import Data.List (sort)
import Text.Show.Functions()

-- Question 1: x == 2 * half x
-- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Double -> Double
halfIdentity = (*2) . half

-- Question 2: List is sorted
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

-- Question 3a: Addition is associative
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

-- Question 3b: Addition is commutative
plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

-- Question 4a: Multiplication is associative
timesAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
timesAssociative x y z =
  x * (y * z) == (x * y) * z

-- Question 4b: Multiplication is commutative
timesCommutative :: (Eq a, Num a) => a -> a -> Bool
timesCommutative x y =
  x * y == y * x

-- Question 5a: Quot/rem identity
quotRemIdentity :: Integral a => a -> a -> Bool
quotRemIdentity _ 0 = True
quotRemIdentity x y = (quot x y)*y + (rem x y) == x

-- Question 5b: Div/mod identity
divModIdentity :: Integral a => a -> a -> Bool
divModIdentity _ 0 = True
divModIdentity x y = (div x y)*y + (mod x y) == x

-- Question 6a: Exponents are not associative
expAssociative :: Integral a => a -> a -> a -> Bool
expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

-- Question 6b: Exponents are not commutative
expCommutative :: Integral a => a -> a -> Bool
expCommutative x y =
  x ^ y == y ^ x

-- Question 7: List reversal is its own inverse
listReverse :: Eq a => [a] -> Bool
listReverse xs = (reverse . reverse) xs == id xs

-- Question 8a: Dollar operator definition
dollarDefinition :: Eq b => (a -> b) -> a -> Bool
dollarDefinition f a = (f $ a) == f a

-- Question 8b: Compose operator definition
composeDefinition :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
composeDefinition f g x = (f . g) x == (\y -> f (g y)) x

-- Question 8c: Function composition is associative
composeAssociative ::
  Eq d => (c -> d) -> (b -> c) -> (a -> b) -> a -> Bool
composeAssociative f g h x = ((f . g) . h) x == (f . (g . h)) x

-- Question 9a: foldr (:) == (++)
combine :: Eq a => [a] -> [a] -> Bool
combine xs ys = foldr (:) xs ys == ys ++ xs

-- Question 9b: foldr (++) [] == concat
concatenate :: Eq a => [[a]] -> Bool
concatenate xs = foldr (++) [] xs == concat xs

-- Question 10: takeLength (falsifiable)
takeLength :: Int -> [a] -> Bool
takeLength n xs = length (take n xs) == n

-- Question 11: read/show
readShow :: (Show a, Read a, Eq a) => a -> Bool
readShow x = (read (show x)) == x

main :: IO ()
main = do
  putStrLn "Question 1: x == 2 * half x"
  quickCheck (\x -> x == halfIdentity x)

  putStrLn "Question 2: List is sorted"
  quickCheck (listOrdered . sort :: [Int] -> Bool)

  putStrLn "Question 3a: Addition is associative"
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)

  putStrLn "Question 3b: Addition is commutative"
  quickCheck (plusCommutative :: Int -> Int -> Bool)

  putStrLn "Question 4a: Multiplication is associative"
  quickCheck (timesAssociative :: Int -> Int -> Int -> Bool)

  putStrLn "Question 4b: Multiplication is commutative"
  quickCheck (timesCommutative :: Int -> Int -> Bool)

  putStrLn "Question 5a: Quot/rem identity"
  quickCheck (quotRemIdentity :: Int -> Int -> Bool)

  putStrLn "Question 5b: Div/mod identity"
  quickCheck (divModIdentity :: Int -> Int -> Bool)

  putStrLn "Question 6a: Exponents are not associative:"
  quickCheck (expAssociative :: Int -> Int -> Int -> Bool)

  putStrLn "Question 6b: Exponents are not commutative"
  quickCheck (expCommutative :: Int -> Int -> Bool)

  putStrLn "Question 7: List reversal is its own inverse"
  quickCheck (listReverse :: [Int] -> Bool)
  
  putStrLn "Question 8a: Dollar operator definition"
  quickCheck (dollarDefinition :: (Int -> Int) -> Int -> Bool)

  putStrLn "Question 8b: Compose operator definition"
  quickCheck (composeDefinition ::
                 (Int -> Int) -> (Int -> Int) -> Int -> Bool)
  
  putStrLn "Question 8c: Function composition is associative"
  quickCheck (composeAssociative ::
                 (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Int -> Bool)

  putStrLn "Question 9a: foldr (:) == (++)"
  quickCheck (combine :: [Int] -> [Int] -> Bool)
  
  putStrLn "Question 9b: foldr (++) [] == concat"
  quickCheck (concatenate :: [[Int]] -> Bool)
  
  putStrLn "Question 10: takeLength (falsifiable)"
  quickCheck (takeLength :: Int -> [Int] -> Bool)

  putStrLn "Question 11: read/show"
  quickCheck (readShow :: Int -> Bool)
  