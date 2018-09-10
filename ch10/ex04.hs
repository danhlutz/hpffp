-- ex04.hs
-- Chapter Exercises 1
module LetterSounds where

import Data.List (intersperse)

stops  = "pbdtkg"
vowels = "aeiou"

svs :: [a] -> [a] -> [[a]]
svs l1 l2 = [ s1 : v : s2 : [] | s1 <- l1,
                                 v  <- l2,
                                 s2 <- l1]

svsPs :: [Char] -> [Char] -> [[Char]]
svsPs l1 l2 = [s1 : v : s2 : [] | s1 <- l1,
                                  v  <- l2,
                                  s2 <- l1,
                                  s1 == 'p']

nouns = ["yogurt", "union", "gotv", "election", "lego"]
verbs = ["stuffs", "eats", "gotvs", "campaigns", "recurses"]

makeSentences :: [String] -> [String] -> [String]
makeSentences n v = map (concat . (intersperse " ")) (svs n v)
