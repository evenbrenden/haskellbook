module Chapter21Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Traversable instances

-- Identity

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity x) = f x
    foldr f z (Identity x) = f x z

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

testIdentity :: IO ()
testIdentity = quickBatch (traversable trigger)
    where
        trigger :: Identity (Int, Int, [Int])
        trigger = undefined

-- Constant

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse f (Constant x) = Constant <$> pure x

instance Eq a => EqProp (Constant a b) where (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

testConstant :: IO ()
testConstant = quickBatch (traversable trigger)
    where
        trigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
        trigger = undefined

-- Optional

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Yep x) = f x

instance Traversable Optional where
    traverse f Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (1, return Nada), (10, return (Yep a)) ]

instance Eq a => EqProp (Optional a) where (=-=) = eq

testOptional :: IO ()
testOptional = quickBatch (traversable trigger)
    where
        trigger :: Optional (Int, Int, [Int])
        trigger = undefined

-- List

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap x Nil = Nil
  fmap x (Cons a l) = Cons (x a) (fmap x l)

instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons a l) = f a <> foldMap f l

instance Traversable List where
    traverse f Nil = pure Nil
    traverse f (Cons a l) = Cons <$> f a <*> traverse f l

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toMyList <$> arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

testList :: IO ()
testList = quickBatch (traversable trigger)
    where
        trigger :: List (Int, Int, [Int])
        trigger = undefined

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
    foldMap f (Three _ _ z) = f z
    foldr f z0 (Three _ _ z) = f z z0

instance Traversable (Three a b) where
    traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

testThree :: IO ()
testThree = quickBatch (traversable trigger)
    where
        trigger :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
        trigger = undefined

-- Pair

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
    foldMap f (Pair _ y) = f y
    foldr f z0 (Pair _ y) = f y z0

instance Traversable (Pair a) where
    traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

testPair :: IO ()
testPair = quickBatch (traversable trigger)
    where
        trigger :: Pair (Int, Int, [Int]) (Int, Int, [Int])
        trigger = undefined

-- Big

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
    foldMap f (Big _ y z) = f y <> f z

instance Traversable (Big a) where
    traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

testBig :: IO ()
testBig = quickBatch (traversable trigger)
    where
        trigger :: Big (Int, Int, [Int]) (Int, Int, [Int])
        trigger = undefined

-- Bigger

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger x y z w) = Bigger x (f y) (f z) (f w)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ y z w) = f y <> f z <> f w

instance Traversable (Bigger a) where
    traverse f (Bigger x y z w) = Bigger x <$> f y <*> f z <*> f w

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Bigger a b c d

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

testBigger :: IO ()
testBigger = quickBatch (traversable trigger)
    where
        trigger :: Bigger (Int, Int, [Int]) (Int, Int, [Int])
        trigger = undefined

-- S SKIPPED

-- Instances for Tree SKIPPED
