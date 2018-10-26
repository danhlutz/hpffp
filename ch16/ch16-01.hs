-- ch16-01.hs
module Practice where

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatIsThisCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatIsThisCalled = WhatIsThisCalled
  fmap f (Matter a) = Matter (f a)

data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

-- NOT OK!
instance Functor CountingBad where
  fmap f (Heisenberg n a) =
      (Heisenberg (n+1) (f a))

data CountingGood a =
  Heise Int a 
  deriving (Eq, Show)

instance CountingGood where
  fmap f (Heise n a) = Heise (n) (f a)
