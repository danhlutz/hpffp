--ex07.hs
module LetterManipulators where

thirdLetter :: [Char] -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex x = 
        let s = "Curry is awesome!"
            in s !! x
