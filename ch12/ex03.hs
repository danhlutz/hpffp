-- ex03.hs
module Exercise where

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = elem x vowels

countVowels :: Num a => String -> a
countVowels "" = 0
countVowels (x:xs)
  | isVowel x = 1 + countVowels xs
  | otherwise = countVowels xs

consonants :: String -> Int
consonants str = (length str) - (countVowels str)

mkWord :: String -> Maybe Word'
mkWord str
  | (countVowels str) > (consonants str) = Nothing
  | otherwise = Just (Word' str)
