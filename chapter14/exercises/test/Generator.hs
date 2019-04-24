-- Chapter 14: Testing
-- Make a Gen random generator for the datatype (page 565)
module Generator where

import Test.QuickCheck

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

-- Question 1: Equal probabilities for each
genFoolEqual :: Gen Fool
genFoolEqual = oneof [return Fulse, return Frue]

-- Question 2: 2/3s chance of Fulse, 1/3 chance of Frue
genFoolMoreFulse :: Gen Fool
genFoolMoreFulse =
  frequency [ (2, return Fulse)
            , (1, return Frue)]

main :: IO ()
main = do
  putStrLn "GenFoolEqual"
  sample genFoolEqual

  putStrLn "GenFoolMoreFulse"
  sample genFoolMoreFulse