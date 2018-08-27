-- ex11.hs
module FilterPractice where

-- 1)
filterThrees :: Integral a => [a] -> [a]
filterThrees xs = filter (\x -> rem x 3 == 0) xs

-- 2) 
howManyThrees :: Integral a => [a] -> Int
howManyThrees = length . filterThrees

-- 3)
isArticle :: String -> Bool
isArticle x = elem x ["an", "a", "the"]

skipDrop :: Char -> String -> String
skipDrop c sent
  | sent == "" = []
  | otherwise  =
      dropWhile (==c) (dropWhile (/=c) sent)

myWords :: String -> [String]
myWords sent
  | sent == "" = []
  | otherwise   =
        (takeWhile (/=' ') sent)
      : (myWords (skipDrop ' ' sent))

noArticles :: String -> [String]
noArticles sent = 
  filter isNotArticle (myWords sent)
  where isNotArticle x = not (isArticle x)
