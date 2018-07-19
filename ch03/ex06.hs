--chapterFun.hs
module ChapterFun where

--Given "Curry is awesome"
--Return "Curry is awesome!"
addEmphasis :: String -> String
addEmphasis s = s ++ "!"

--Given "Curry is awesome!"
--Return "y"
getFourth :: String -> String
getFourth s = [s !! 4]

--Given "Curry is awesome!"
--Return "awesome!"
dropNine :: String -> String
dropNine s = drop 9 s

testString :: String
testString = "Curry is awesome!"
