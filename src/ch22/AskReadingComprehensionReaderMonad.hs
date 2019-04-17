{-# LANGUAGE InstanceSigs #-}

module AskReadingComprehensionReaderMonad where

import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 abc fa fb = abc <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ (\r -> a)
    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) =
        Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> Reader r b
    (Reader ra) >>= aRb =
        Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy

person :: Person
person =
    Person  (HumanName "Big Bird")
            (DogName "Barkley")
            (Address "Sesame Street")

dawg :: IO()
dawg = do
    print $ runReader getDogRM person
