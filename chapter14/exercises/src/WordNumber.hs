-- Chapter 14: Testing
-- Validating numbers into words (pages 561-562)
module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = digitWords !! n
  where digitWords = ["zero", "one", "two", "three", "four"
                     , "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = intercalate "-" $ fmap digitToWord (digits n)
