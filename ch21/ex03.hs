-- ch21/ex03.hs

module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Testing Tree a"
  quickBatch $ functor treeCheck
  quickBatch $ traversable treeCheck

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

splitList :: [a] -> ([a], [a])
splitList items = iter items 0 ([], [])
 where iter :: [a] -> Int -> ([a], [a]) -> ([a], [a])
       iter [] _ (as, bs) = (as, bs)
       iter (x:xs) 0 (as, bs) = iter xs 1 (x:as, bs)
       iter (x:xs) _ (as, bs) = iter xs 0 (as, x:bs)

listToTree :: [a] -> Tree a
listToTree [] = Empty
listToTree (x:[]) = Leaf x
listToTree (x:xs) = Node (listToTree first) x (listToTree second)
  where splitUp = splitList xs
        first = fst splitUp
        second = snd splitUp

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    return $ listToTree x

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

treeCheck :: Tree (Int, Int, [Int])
treeCheck = undefined

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node x y z) =
    mappend (foldMap f x) $ mappend (f y) (foldMap f z)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node x y z) =
    Node <$> (traverse f x) <*> (f y) <*> (traverse f z)
