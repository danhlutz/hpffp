-- ex09.hs
module TreeTime where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- write map
mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left)
       (f a)
       (mapTree f right)

-- tests
testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- convert to list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = 
  x : ((preorder left) ++ (preorder right))

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) =
  (inorder left) ++ (x : (inorder right))

postorder :: BinaryTree a -> [a]
postorder = undefined

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInOrder :: IO ()
testInOrder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears"

-- foldTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x tree = foldr f x (inorder tree)

testFold :: IO ()
testFold =
  if foldTree (+) 0 testTree == 6
  then putStrLn "foldTree works!"
  else putStrLn "Try again!"
