-- Chapter 14: Testing
-- Validating ciphers (page 574)
module CipherTest where
import Cipher
import Test.QuickCheck

genChar :: Gen Char
genChar = elements $ ['A'..'Z'] ++ ['a'..'z']

genString :: Gen String
genString = listOf genChar

caesarGen :: Gen (Int, String)
caesarGen = do
  shift <- arbitrary
  message <- genString
  return (shift, message)

caesarAndBack :: Property
caesarAndBack = forAll caesarGen (\(shift, message) ->
  unCaesar shift (caesar shift message) == message)

vigenereGen :: Gen (String, String)
vigenereGen = do
  k <- genChar
  ks <- genString
  message <- genString
  return ((k:ks), message)

vigenereAndBack :: Property
vigenereAndBack = forAll vigenereGen (\(key, message) ->
  unVigenere key (vigenere key message) == message)

main :: IO ()
main = do
  putStrLn "Caesar tests"
  quickCheck caesarAndBack

  putStrLn "Vigenere tests"
  quickCheck vigenereAndBack
