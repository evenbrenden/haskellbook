module Fool where

import Test.QuickCheck

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = elements [Fulse, Frue]

foolGenUnequal :: Gen Fool
foolGenUnequal = frequency [(2, return Fulse), (1, return Frue)]

instance Arbitrary Fool where
  arbitrary = foolGenUnequal

-- sample (arbitrary :: Gen Fool)
