-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- Question 5
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- Question 6
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

main = do
  print (roundTrip 4)
  print (id 4)
  print (roundTripPF 4)
  print ((roundTrip' 4) :: Int)
