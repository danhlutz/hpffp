-- ch17/ex04.hs

module Exercise where

-- 1
x :: Maybe String
x = const <$> Just "Hello" <*> pure "World"

-- 2
y :: Maybe (Int, Int, String, [Int])
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

y' :: Maybe (Int, Int, String, [Int])
y' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Nothing
