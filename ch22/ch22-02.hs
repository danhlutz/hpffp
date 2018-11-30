-- ch22-02.hs

module Practice where

import Control.Applicative (liftA2)

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

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

-- more explicit
(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' =
  Dog <$->> dogName <*->> address

--
getDogR'' :: Person -> Dog
getDogR'' =
  liftA2 Dog dogName address
