-- ex05.hs
module Secret where

-- 2)

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3)
betterSeekrit :: Fractional a => String -> a
betterSeekrit x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
