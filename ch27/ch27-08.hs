-- ch27-08.hs

module Practice where

strictPattern :: (a, b) -> String
strictPattern (a, b) = const "Cousin it" a

lazyPattern :: (a, b) -> String
-- the ~ makes the pattern match lazy
lazyPattern ~(a, b) = const "Cousin It" a
