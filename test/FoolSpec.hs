module FoolSpec where

import Test.Hspec
import Test.QuickCheck
import Fool

spec :: Spec
spec = do
  describe "Fool tests" $ do
    it "Fool is Fool" $ do
      property $ \x -> x == (x :: Fool)
