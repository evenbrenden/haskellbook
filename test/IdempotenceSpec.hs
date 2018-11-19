module IdempotenceSpec where

import Test.Hspec
import Test.QuickCheck
import E11
import Data.List (sort)

spec :: Spec
spec = do
  describe "Capitalization is idempotent" $ do
    let twice f = f . f
    let quice = twice . twice
    it "capitalizeWord twice and four times is capitalizeWord" $ do
      property $ \x -> (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == quice capitalizeWord x)
  describe "Sorting is idempotent" $ do
    let twice f = f . f
    let quice = twice . twice
    it "sort twice and four times is sort" $ do
      property $ \x -> (sort x == twice sort x) && (sort x == quice sort (x :: [Double]))
