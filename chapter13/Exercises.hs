-- Chapter 13: Building Projects
import Data.Char (chr, ord, isLower, isUpper, toLower, isAlphaNum)
import Control.Monad (forever)
import System.Exit (exitSuccess)

-- Intermission: Check your understanding (page 502)
-- Question 1: forever, when
-- Question 2: Data.Bits, Database.Blacktip.Types
-- Question 3: Database data types
-- Question 4a: Control.Concurrent.MVar,
--              Filesystem.Path.CurrentOS,
--              Control.Concurrent
-- Question 4b: Filesystem
-- Question 4c: Control.Monad

-- Modifying code (pages 529-531)
-- Question 1a: Caesar
caesar :: Int -> String -> String
caesar offset = map shiftLetter
  where shiftLetter c
          | isUpper c = chr (mod (ord c - ord 'A' + offset) 26 + ord 'A')
          | isLower c = chr (mod (ord c - ord 'a' + offset) 26 + ord 'a')
          | otherwise = c

unCaesar :: Int -> String -> String
unCaesar offset cs = caesar (-offset) cs

encryptCaesar :: IO ()
encryptCaesar = do
  putStr "Enter number of chars to shift: "
  offset <- getLine
  putStr "Enter message to encrypt: "
  message <- getLine
  putStrLn $ caesar (read offset) message

decryptCaesar :: IO ()
decryptCaesar = do
  putStr "Enter number of chars to shift: "
  offset <- getLine
  putStr "Enter encrypted message: "
  cypher <- getLine
  putStrLn $ unCaesar (read offset) cypher

-- Question 1b: Vigenere
type KeyText     = String
type MessageText = String
type CypherText  = String

vigenere :: KeyText -> MessageText -> CypherText
vigenere ks ms = zipWith f (cycle ks) ms
  where f k m  = shiftLetter (offset k) m

unVigenere :: KeyText -> CypherText -> MessageText
unVigenere ks cs = zipWith f (cycle ks) cs
  where f k c = shiftLetter (-offset k) c

shiftLetter :: Int -> Char -> Char
shiftLetter offset ch
  | isUpper ch = chr (mod (ord ch - ord 'A' + offset) 26 + ord 'A')
  | isLower ch = chr (mod (ord ch - ord 'a' + offset) 26 + ord 'a')
  | otherwise = ch

offset :: Char -> Int
offset k
  | isUpper k = ord k - ord 'A'
  | isLower k = ord k - ord 'a'
  | otherwise = 0

encryptVigenere :: IO ()
encryptVigenere = do
  putStr "Enter encryption key: "
  key <- getLine
  putStr "Enter message to encrypt: "
  message <- getLine
  putStrLn $ vigenere key message

decryptVigenere :: IO ()
decryptVigenere = do
  putStr "Enter decryption key: "
  key <- getLine
  putStr "Enter encrypted message: "
  cypher <- getLine
  putStrLn $ unVigenere key cypher

-- Question 2
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
               putStrLn "Nope!"
               exitSuccess

-- Question 3
palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
     True -> putStrLn "It's a palindrome!"
     False -> do
                putStrLn "Nope!"
                exitSuccess

isPalindrome :: String -> Bool
isPalindrome line1 =
  let line2 = filter (isAlphaNum) line1
      line3 = map (toLower) line2
  in line3 == reverse line3

-- Question 4
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine
  case mkPerson name (read age) of
    Right person ->
      putStrLn ("Yay! Successfully got a person: "  ++ show person)
    Left invalid ->
      putStrLn ("An error occurred: " ++ show invalid)
