-- Chapter 14: Testing
-- Failure (page 571)
module Failure where

import Test.QuickCheck

square :: Double -> Double
square x = x * x

squareIdentity :: Double -> Bool
squareIdentity x = (square . sqrt) x == x

main :: IO ()
main = do
  -- Fails due to limited precision and can't
  -- take the square root of a negative number
  putStrLn "squareIdentity (falsifiable)"
  quickCheck squareIdentity
