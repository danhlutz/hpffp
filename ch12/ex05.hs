-- ex05.hs
module Exercise where

-- 1)
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- 2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just y) = f y

-- 3)
fromMaybe :: a -> Maybe a -> a
-- fromMaybe x Nothing  = x
-- fromMaybe _ (Just y) = y
fromMaybe x y = mayybee x id y

-- 4)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just y):ys) = y : (catMaybes ys)

-- 6)
flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe ((Just x):xs)
  | rest == Nothing = Nothing
  | otherwise       = Just (x : getJustVal rest)
  where rest = flipMaybe xs
  
getJustVal (Just x) = x
