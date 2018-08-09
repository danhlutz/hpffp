--ex07.hs
--does it typecheck?
module DoesItCheck where

-- 1)

data Person = Person Bool
-- to fix add this
  deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- this will not work because there is no instance of Show for Person

-- 2)
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x
-- this will not work because there is not instance of Eq for Mood

-- 3)
-- a) settleDown accepts either Woot or Blah
-- b) settleDown 9 will error because settleDown :: Mood -> Mood
-- c) Blah > Woot will error because Mood has no instance of Ord

-- 4)
type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object
  deriving (Eq, Show)

--s1 = Sentence "dogs" "drool"
-- s1 won't check because a sentence requires an object
s1 = Sentence "dogs" "drool" "saliva"
s2 = Sentence "Julie" "loves" "dogs"
-- s2 will type check
