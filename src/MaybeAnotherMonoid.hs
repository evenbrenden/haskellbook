module MaybeAnotherMonoid where

-- Chapter 15

import Data.Semigroup
import Data.Monoid
import Test.QuickCheck
import MonoidQuickCheckHelpers
import Optional

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) x (First' Nada) = x
  (<>) (First' Nada) y = y
  (<>) x y = x

-- https://stackoverflow.com/questions/40089769/making-an-arbitrary-instance-for-a-newtype-that-uses-maybe

first'Gen :: Arbitrary a
          => Gen (First' a)
first'Gen = do
  a <- optionalGen
  return (First' a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = first'Gen

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: FirstMappend)
  quickCheck (mli :: FstId)
  quickCheck (mri :: FstId)
