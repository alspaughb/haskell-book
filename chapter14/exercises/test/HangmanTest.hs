-- Chapter 14: Testing
-- Hangman testing (pages 565-566)
module HangmanTest where

import Test.Hspec
import Hangman

-- Initial puzzle
puzzle1 :: Puzzle
puzzle1 = Puzzle "puzzle"
            [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] []

-- Initial puzzle after guessing a 'z'
puzzle2 :: Puzzle
puzzle2 = Puzzle "puzzle"
            [Nothing, Nothing, Just 'z', Just 'z', Nothing, Nothing] ['z']

-- Initial puzzle after incorrectly guessing 'r'
puzzle3 :: Puzzle
puzzle3 = Puzzle "puzzle"
            [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] ['r']

main :: IO ()
main = hspec $ do
  describe "Hangman" $ do
    it "Puzzle contains 'z'" $ do
      fillInCharacter puzzle1 'z' `shouldBe` puzzle2
    it "Guess 'z' in puzzle" $ do
      puzzle <- handleGuess puzzle1 'z'
      puzzle `shouldBe` puzzle2
    it "Guess 'r' not in puzzle" $ do
      puzzle <- handleGuess puzzle1 'r'
      puzzle `shouldBe` puzzle3
    it "Guess 'z' which was already guessed" $ do
      puzzle <- handleGuess puzzle2 'z'
      puzzle `shouldBe` puzzle2
