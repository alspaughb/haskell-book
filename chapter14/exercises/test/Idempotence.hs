-- Chapter 14: Testing
-- Idempotence (pages 564-565)
module Idempotence where

import Data.Char
import Data.List
import Test.QuickCheck

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

-- Question 1: CapitalizeWord is idempotent
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeTwice :: String -> Bool
capitalizeTwice x = capitalizeWord x == twice capitalizeWord x

capitalizeFourTimes :: String -> Bool
capitalizeFourTimes x =
  twice capitalizeWord x == fourTimes capitalizeWord x

-- Question 2: Sort is idempotent
sortTwice :: Ord a => [a] -> Bool
sortTwice xs = sort xs == twice sort xs

sortFourTimes :: Ord a => [a] -> Bool
sortFourTimes xs =
  twice sort xs == fourTimes sort xs

main :: IO ()
main = do
  putStrLn "CapitalizeTwice"
  quickCheck capitalizeTwice

  putStrLn "CapitalizeFourTimes"
  quickCheck capitalizeFourTimes

  putStrLn "SortTwice"
  quickCheck (sortTwice :: [Int] -> Bool)

  putStrLn "SortFourTimes"
  quickCheck (sortFourTimes :: [Int] -> Bool)