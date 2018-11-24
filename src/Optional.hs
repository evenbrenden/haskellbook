module Optional where

import Test.QuickCheck
import Control.Monad

-- Chapter 15

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

instance Semigroup a
      => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) x Nada = x
  (<>) Nada y = y
  (<>) (Only x) (Only y) = Only $ (<>) x y

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

optionalGen :: Arbitrary a =>
               Gen (Optional a)
optionalGen = do
  a <- arbitrary
  frequency [ (1, return Nada), (10, return (Only a)) ]
