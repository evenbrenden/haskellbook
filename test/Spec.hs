import Test.Hspec
import E14

-- E14: Intermission: Short Exercise --

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "15 multiplied by 3 is 45" $ do
      multi 15 3 `shouldBe` 45
    it "4 multiplied by 4 is 16" $ do
      multi 4 4 `shouldBe` 16
