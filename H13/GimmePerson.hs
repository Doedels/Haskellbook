module GimmePerson where

import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvallidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
       | name /= "" && age > 0 = Right $ Person name age
       | name == "" = Left NameEmpty
       | not (age > 0) = Left AgeTooLow
       | otherwise = Left $ PersonInvallidUnknown $
         "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Give your name: "
  name <- getLine
  putStr "Give your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
    Right person -> putStrLn $ "Yay! Succesfully got a person: " ++ show person
    Left err -> putStrLn $ "An errror occured: " ++ show err
