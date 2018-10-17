module Main where

import Hangman
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "testing guesses recorded"
  quickCheck prop_guessRecorded
  putStrLn "testing answer handled"
  quickCheck prop_answerHandled

-- test fillInCharacter

instance Arbitrary Puzzle where
  arbitrary = puzzleGen

puzzleGen :: Gen Puzzle
puzzleGen = do
  w <- arbitrary
  return (freshPuzzle w)

getGuesses (Puzzle _ _ guesses _) = guesses

getAnswer (Puzzle _ a _ _) = a

getWord (Puzzle w _ _ _) = w

getCount :: Puzzle -> Int
getCount (Puzzle _ _ _ i) = i

prop_guessRecorded :: Puzzle -> Char -> Bool
prop_guessRecorded (Puzzle w a guesses i) c =
  getGuesses (fillInCharacter (Puzzle w a guesses i) c) == (c:guesses)

prop_answerHandled :: Puzzle -> Char -> Bool
prop_answerHandled puzzle c =
     (getWord filledIn) == []
  || if elem c (getWord filledIn)
     then elem (Just c) (getAnswer filledIn)
     else elem Nothing (getAnswer filledIn)
  where filledIn = fillInCharacter puzzle c
