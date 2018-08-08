--ch06-03.hs
--Show
module ShowPractice where

data Mood = Blah

instance Show Mood where
  show _ = "Blah"
