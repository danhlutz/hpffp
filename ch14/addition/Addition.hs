-- Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      shouldBe (2 + 2) 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

    it "2 times 10 is 20" $ do
      multiply 2 10 `shouldBe` 20
    it "0 times 10 is 0" $ do
      multiply 0 10 `shouldBe` 0
    it "5 times 0 is 0" $ do
      multiply 5 0 `shouldBe` 0
    it "(-3) times 5 is 15" $ do
      multiply (-3) 5 `shouldBe` (-15)
    it "2 times (-13) should be (-26)" $ do
      multiply 2 (-13) `shouldBe` (-26)
    it "(-2) times (-3) is 6" $ do
      multiply (-2) (-3) `shouldBe` 6
--  QuickCheck tests moved to separate function
--  it "x + 1 is always greater than x" $ do
--    property $ \x -> x + 1 > (x :: Int)

-- QuickCheck

runQc :: IO ()
runQc = quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

-- 

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = 
             go (n - d) d (count + 1)

multiply :: (Eq a, Num a, Ord a) => a -> a -> a
multiply x y = go x y 0
  where go c d prod
         | d == 0 = prod
         | d < 0  = go (negate c) (negate d) prod
         | otherwise = go c (d - 1) (prod + c) 

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
