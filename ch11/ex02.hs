-- ex02.hs
module Ex02 where

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

-- some test data
myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 200)

-- 1) myCar :: Vehicle

-- 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3)
getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man

-- 4) getManu will return an exception if run on a Plane

-- 5)
getSize :: Vehicle -> Size
getSize (Plane _ size) = size
