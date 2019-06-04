module CipherSpec where

import Test.QuickCheck
import Test.Hspec
import Cipher (caesar, unCaesar, vigenere, unVigenere)

-- https://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
  deriving Show

instance Arbitrary SafeString where
  arbitrary = fmap SafeString genSafeString

-- If our cipher had supported numbers and symbols we could have used Test.QuickCheck.PrintableCharacter

prop_caesar :: SafeString -> Int -> Bool
prop_caesar x y = unCaesar (caesar (unwrapSafeString x) y) y == unwrapSafeString x

prop_vigenere :: SafeString -> SafeString -> Bool
prop_vigenere x y = unVigenere (vigenere (unwrapSafeString x) (unwrapSafeString y)) (unwrapSafeString y) == unwrapSafeString x

spec :: Spec
spec = do
  describe "Ciphers" $ do
    it "Deciphered Caesar cipher is original string" $ do
      quickCheck prop_caesar
    it "Deciphered Vigenere cipher is original string" $ do
      quickCheck prop_vigenere
