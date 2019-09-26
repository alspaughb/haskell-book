module Reverse where

rsrv :: String -> String
rsrv s =
  awesome ++ " " ++ is ++ " " ++ curry
  where curry = take 5 s
        d1 = drop 6 s
        is = take 2 d1
        d2 = drop 3 d1
        awesome = take 7 d2

main :: IO ()
main = print $ rsrv "Curry is awesome"

