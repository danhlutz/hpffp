-- ex08.hs
module Exercise where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1)
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = Node (unfold f third) second (unfold f third)
  where result = getMaybeVal (f x)
        second = getSecond result
        third  = getThird result

getMaybeVal :: Maybe a -> a
getMaybeVal (Just x) = x

getSecond :: (a, b, c) -> b
getSecond (_, x, _) = x

getThird :: (a, b, c) -> c
getThird (_, _, x) = x

-- 2)
treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild x = Node (treeBuild (x - 1)) x (treeBuild (x - 1))
