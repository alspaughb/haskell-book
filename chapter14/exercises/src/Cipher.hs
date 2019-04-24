-- Chapter 14: Testing
-- Validating ciphers (page 567)
module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar shift = map (shiftLetter shift)

unCaesar :: Int -> String -> String
unCaesar shift = caesar (-shift)

vigenere :: String -> String -> String
vigenere ks ms = zipWith f (cycle ks) ms
  where f k m = shiftLetter (offset k) m

unVigenere :: String -> String -> String
unVigenere ks cs = zipWith f (cycle ks) cs
  where f k c = shiftLetter (-offset k) c

shiftLetter :: Int -> Char -> Char
shiftLetter shift ch
  | isUpper ch = chr (mod (ord ch - ord 'A' + shift) 26 + ord 'A')
  | isLower ch = chr (mod (ord ch - ord 'a' + shift) 26 + ord 'a')
  | otherwise  = ch

offset :: Char -> Int
offset k
  | isUpper k = ord k - ord 'A'
  | isLower k = ord k - ord 'a'
  | otherwise = 0

