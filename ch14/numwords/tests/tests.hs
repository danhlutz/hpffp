module Main where

import Numwords
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "NumWords" $ do
    it "numToWord 1 is one" $ do
      shouldBe (numToWord 1) "one"
    it "numToWord 9 is nine" $ do
      shouldBe (numToWord 9) "nine"
    it "numToWord 10 is 'one zero'" $ do
      shouldBe (numToWord 10) "one zero"
    it "numToWord 987 is 'nine eight seven'" $ do
      shouldBe (numToWord 987) "nine eight seven"
    it "numToWord (-13) is 'negative one three'" $ do
      shouldBe (numToWord (-13)) "negative one three"
    it "wordToNum 'five' is 5" $ do
      shouldBe (wordToNum "five") 5
    it "wordToNum 'negative five' is (-5)" $ do
      shouldBe (wordToNum "negative five") (-5)
    it "wordToNum 'seven eight three' is 783" $ do
      shouldBe (wordToNum "seven eight three") 783
    it "wordToNum 'negative eight three' is (-83)" $ do
      shouldBe (wordToNum "negative eight three") (-83)

-- QuickCheck tests
runQc :: IO ()
runQc = quickCheck prop_reversible

prop_reversible :: Integer -> Bool
prop_reversible x = wordToNum (numToWord x) == x
