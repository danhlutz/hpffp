-- ch22/ex04.hs

{-# LANGUAGE InstanceSigs #-}

module Exercise where

newtype Reader r a =
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> id a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \ r -> rab r (ra r)

instance Monad (Reader r) where
  return x = pure x

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

--

getDog :: Reader Person Dog
getDog = do
  dog <- Reader dogName
  address' <- Reader address
  return $ Dog dog address'

getDog' :: Person -> Dog
getDog' = (runReader getDog)

--

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris")
               (DogName "Papu")
               (Address "Austin")

