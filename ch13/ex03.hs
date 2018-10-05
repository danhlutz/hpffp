-- ex03.hs
module Exercise where

gimmePerson :: IO ()
gimmePerson = do
  name <- getName
  age  <- getAge
  announcePerson (mkPerson name age)

getName :: IO String
getName = do
  putStr "Please enter a name: "
  name <- getLine
  return name

getAge :: IO Integer
getAge = do
  putStr "Please enter their age: "
  age <- getLine
  return (read age)

announcePerson :: Either PersonInvalid Person -> IO ()
announcePerson (Right person) = do
  putStrLn $ "Yay! You successfuly got a person: " ++ show person
announcePerson (Left error) = do
  putStrLn $ "ERROR! " ++ show error

-- DO NOT CHANGE ANYTHING BELOW HERE
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age
