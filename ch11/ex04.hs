-- ex04.hs
module ForExample where

data Example = MakeExample deriving Show

-- 1) MakeExample :: Example
--    
--    asking for the type of Example raises an error

-- 2)

data MyExample = MakeMyExample Integer deriving Show

-- MakeMyExample :: Integer -> MyExample
-- this is very similar to the type of a function that takes one argument
