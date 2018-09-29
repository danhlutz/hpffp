-- ex07.hs
module Exercise where

-- 1) myIterate
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f (f x))

-- 2) myUnfoldr
--    > take 10 $ unfoldr (\b -> Just (b, b+1)) 0
--    > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = y : (myUnfoldr f z)
  where result = getMaybeVal (f x)
        y = fst result
        z = snd result

getMaybeVal :: Maybe a -> a
getMaybeVal (Just x) = x

-- 3) 
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\ x -> Just (x, (f x))) x
