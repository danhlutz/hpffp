-- ch12-03.hs
module Practice where

data Example a = Blah | RoofGoats | Woot a

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

data Trivial = Trivial deriving Show

data UnaryC = UnaryC Int deriving Show

data Unary a = Unary a deriving Show
