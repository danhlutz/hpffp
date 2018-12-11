-- ch24/ex03.hs
{-# LANGUAGE QuasiQuotes #-}

module Exercise where

import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

testData :: String
testData = [r|
3/4
12.5
100/23
23.07876
0.001
|]

type FractionOrDouble = Either Rational Double

parseFrac :: Parser Rational
parseFrac = do
  num <- integer
  _ <- char '/'
  denom <- integer
  return (num % denom)

parserDouble :: Parser Double
parserDouble = do
  big <- integer
  _ <- char '.'
  zeroes <- many (char '0')
  little <- integer
  return $
      (fromIntegral big :: Double)
    + (mkMinor (fromIntegral little :: Double) (length zeroes))

mkMinor :: Double -> Int -> Double
mkMinor x p
  | x < 1.0   = reducePower x p
  | otherwise = mkMinor (x / 10.0) p

reducePower :: Double -> Int -> Double
reducePower x 0 = x
reducePower x n = reducePower (x / 10.0) (n - 1)

parseFoD :: Parser FractionOrDouble
parseFoD = do
  skipMany (oneOf "\n")
  v <-     try (Left <$> parseFrac)
       <|> try (Right <$> double)
  skipMany (oneOf "\n")
  return v

main :: IO ()
main = do
  let p f i = parseString f mempty i
  print $ p parseFoD "12/16"
  print $ p parseFoD "123.012"
  print $ p (some parseFoD) testData
