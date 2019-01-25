-- queues!
-- functions from Chris Okasaki
-- simple and efficient purely functional queues

module Ex where

data Queue a =
  Queue { leftQ :: [a]
        , rightQ :: [a]
        } deriving (Eq, Show)

emptyQ :: Queue a
emptyQ = Queue [] []

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] [] as = as
rotate [] (r:_) as = r : as
rotate (l:ls) (r:rs) as =
  l : (rotate ls rs (r:as))

makeEq :: [a] -> [a] -> ([a], [a])
makeEq ls rs =
  if length rs <= length ls
  then (ls, rs)
  else ((rotate ls rs []), [])

push :: a -> Queue a -> Queue a
push x (Queue ls rs) = Queue ls' rs'
  where (ls', rs') = makeEq ls (x:rs)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] _) = Nothing
pop (Queue (l:ls) rs) = Just (l, (Queue ls' rs'))
  where (ls', rs') = makeEq ls rs

popToQ :: Queue a -> Maybe (Queue a)
popToQ qs =
  case (pop qs) of
    Nothing -> Nothing
    Just (_, qs') -> Just qs'
