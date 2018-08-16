-- ch07-07.hs
module ChapterPractice where

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

f' :: Int -> [Int] -> Int
f' = foldr (+)

g :: [Char] -> Int
g = length . filter (== 'a')

-- evaluation of g
-- g "abracadabra"
-- length . filter (== 'a') $ "abracadabra"
-- length "aaaaa"
-- 5
