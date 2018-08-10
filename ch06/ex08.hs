--ex08.hs
module TypeKwonDo where

-- 1)
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = 
  (fn x) == y

-- 2)
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith fn x y =
  if x > 0
  then (*2) (fn y)
  else (*3) (fn y)
