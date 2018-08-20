-- ch08-02.hs
module ChapterPractice where

f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing
