module GimmePerson where

import Control.Monad
import System.Exit (exitSuccess)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStr $ "Name: "
  name <- getLine
  putStr $ "Age: "
  age <- getLine
  let age' = read age :: Integer
  let person = mkPerson name age'
  case person of
    (Left (PersonInvalidUnknown invalid)) -> do
      putStrLn $ "Invalid data! " ++ show invalid
    (Left invalid') -> do
      putStrLn $ "Invalid data! " ++ show invalid'
    (Right person') -> do
      putStrLn $ "Yay! " ++ show person'
  exitSuccess
