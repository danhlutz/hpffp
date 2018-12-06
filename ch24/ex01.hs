-- ch24-01.hs

module Practice where

import Text.Trifecta

testParse :: Parser Char -> String -> IO ()
testParse p str =
  print $ parseString p mempty str

testParseStr :: Parser String -> String -> IO ()
testParseStr p str =
  print $ parseString p mempty str

testParseEOF :: Parser () -> String -> IO ()
testParseEOF p str =
  print $ parseString p mempty str

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'

oneDone :: Parser ()
oneDone = one >> eof

oneTwoDone :: Parser ()
oneTwoDone = one >> two >> eof

oneTwoThreeDone :: Parser ()
oneTwoThreeDone = do
  _ <- one
  _ <- two
  _ <- three
  eof

oneStr :: Parser String
oneStr = string "1"

pNL :: String -> IO ()
pNL s = putStrLn ("\n****\n" ++ s)

main :: IO ()
main = do
  pNL "from before the exercise"
  pNL "one:"
  testParse one "123"
  pNL "Exercises"
  pNL "oneDone with input '123' - should fail"
  testParseEOF oneDone "123"
  pNL "oneDone with input '1' - should pass"
  testParseEOF oneDone "1"
  pNL "oneTwoDone with input '123' - should fail"
  testParseEOF oneTwoDone "123"
  pNL "oneTwoDone with input '12' - should pass"
  testParseEOF oneTwoDone "12"
  pNL "oneTwoThreeDone with input '123' - should pass"
  testParseEOF oneTwoThreeDone "123"
  pNL "String Parsers"
  pNL "oneStr with input '123' - should pass"
  testParseStr oneStr "123"
