module UsingQuickCheckSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x
multiAssociative x y z =
  x * (y * z) == (x * y) * z
multiCommutative x y =
  x * y == y * x

quotRemProp _ 0 = True
quotRemProp x y =
  (quot x y)*y + (rem x y) == x
divModProp _ 0 = True
divModProp x y =
  (div x y)*y + (mod x y) == x

expAssociative x y z =
  x ^ (y ^ z)== (x ^ y) ^ z
expCommutative x y =
  x ^ y == y ^ x

dollars :: Eq a => (a -> a)
        -> a
        -> Bool
dollars f a =
  (f $ a) == f a

composed :: Eq a => (a -> a)
         -> (a -> a)
         -> a
         -> Bool
composed f g x =
  (f . g) x == f (g x)

spec :: Spec
spec = do
  describe "half" $ do
    it "half times two is identity" $ do
      let half x = x / 2
      let halfIdentity = (*2) . half
      property $ \x -> halfIdentity x == (x :: Double)
  describe "listOrdered" $ do
    it "sorted list is ordered" $ do
      property ((listOrdered . sort) :: [Int] -> Bool)
  describe "plusAssociative" $ do
    it "addition is associative" $ do
      property (plusAssociative :: Int -> Int -> Int -> Bool)
  describe "plusCommutative" $ do
    it "addition is commutative" $ do
      property (plusCommutative :: Int -> Int -> Bool)
  describe "multiAssociative" $ do
    it "multiplication is associative" $ do
      property (multiAssociative :: Int -> Int -> Int -> Bool)
  describe "multiCommutative" $ do
    it "multiplication is commutative" $ do
      property (multiCommutative :: Int -> Int -> Bool)
  describe "quotRemProp" $ do
    it "quot and rem should work like quotRemProp" $ do
      property (quotRemProp :: Int -> Int -> Bool)
  describe "divModProp" $ do
    it "div and mod should work like divModProp" $ do
      property (divModProp :: Int -> Int -> Bool)
  -- describe "expAssociative" $ do
  --   it "of course not" $ do
  --     property $ \x y z -> expAssociative (x :: Int) (y :: Int) (z :: Int)
  -- describe "expCommutative" $ do
  --   it "it aint so" $ do
  --     property $ \x y -> expCommutative (x :: Int) (y :: Int)
  describe "listReverseReverse" $ do
    it "double reverse is identity" $ do
      property $ \x -> (reverse . reverse) x == (x :: [Int])

  -- CoArbitrary would have been cool for these two --
  describe "dollars" $ do
    it "f $ a is f a" $ do
      property $ \x -> dollars (**1) (x :: Float)
  describe "composed" $ do
    it "(f . g) x is f (g x)" $ do
      property $ \x -> composed (+2) (max 7) (x :: Float)

  -- describe "foldr cons is ++" $ do
  --   it "foldr (:) == (++)" $ do
  --     property $ \x y -> foldr (:) x y == (++) (x :: String) (y :: String)
  describe "foldr (++) [] is concat" $ do
    it "foldr (++) [] == concat" $ do
      property $ \x -> foldr (++) [] x == concat (x :: [String])
  -- describe "Hm. Is that so?" $ do
  --   it "f n xs = length (take n xs) == n" $ do
  --     property $ \n xs -> length (take n (xs :: [Int])) == (n :: Int)
  describe "read-show" $ do
    it "read show is identity" $ do
      property $ \x -> read (show x) == (x :: Bool)
  -- This fails because because we do not have infinte precision => x is rounded
  -- describe "squareIdentity" $ do
  --   it "(sqrt x) * (sqrt x) == x" $ do
  --     let square x = x * x
  --     property $ \x -> (square . sqrt) x == (x :: Float)
