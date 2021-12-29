module E17 where

import Data.List (elemIndex)
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

added :: Maybe Integer
added =
  (+3) <$> (lookup 3 $ zip [1, 2 ,3] [4, 5 ,6])

x1 :: Maybe Integer
x1 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

y1 :: Maybe Integer
y1 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> x1 <*> y1

x2 :: Maybe Int
x2 = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = (liftA2 max') x2 y2

xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

liftedTuple :: Maybe a -> Maybe b -> Maybe (a, b)
liftedTuple = liftA2 (,)

summed :: Maybe Integer
summed = sum <$> liftedTuple x3 y3

-- Exercise: Identity Instance

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Exercise: Constant Instance

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a
      => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)

-- Exercise:: Fixer Upper

fix = const <$> Just "Hello" <*> pure "World"

up = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

-- List Applicative Exercise

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap x Nil = Nil
  fmap x (Cons a l) = Cons (x a) (fmap x l)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f x) (Cons a b) = Cons (f a) (f <$> b) `append` (<*>) x (Cons a b)
  -- https://github.com/vasily-kirichenko/haskell-book/blob/master/src/Applicatives.hs
  -- (<*>) fs as = flatMap ((flip fmap) as) fs

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toMyList <$> arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

type SCI = (String, Char, Integer)
testListTypes :: List SCI
testListTypes = undefined

testList :: IO ()
testList = quickBatch $ applicative testListTypes

-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 1 (Cons x xs) = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith' :: List (a -> b) -> List a -> List b
zipWith' Nil _ = Nil
zipWith' _ Nil = Nil
zipWith' (Cons x y) (Cons u v) = x u `Cons` zipWith' y v

zipWith'' :: (a -> b -> c) -> List a -> List b -> List c
zipWith'' _ _ Nil = Nil
zipWith'' _ Nil _ = Nil
zipWith'' f (Cons x y) (Cons u v) = f x u `Cons` zipWith'' f y v

instance Applicative ZipList' where
  pure x = ZipList' (toMyList . repeat $ x)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith' fs xs)
  -- https://hackage.haskell.org/package/base-4.8.2.0/docs/src/Control.Applicative.html
  -- (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith'' id fs xs)
  -- https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Applicative.html
  -- liftA2 f (ZipList' xs) (ZipList' ys) = ZipList' $ zipWith'' f xs ys

-- (For trying it out in the REPL)
zl' = ZipList' . toMyList
z = zl' [(+9), (*2), (+8), (+100)]
z' = zl' [1..4]
z'' = zl' (repeat 1)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

type LSCI = (List String, List Char, List Integer)
testZipListTypes :: ZipList' SCI
testZipListTypes = undefined

testZipList :: IO ()
testZipList = quickBatch $ applicative testZipListTypes

-- Exercise :: Variations on Either

data Validation e a =
    Failuree e
  | Successs a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failuree e) = Failuree e
  fmap f (Successs a) = Successs (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Successs
  (<*>) (Failuree x) (Failuree y) = Failuree (x <> y)
  (<*>) (Failuree x) (Successs y) = Failuree x
  (<*>) (Successs x) (Failuree y) = Failuree y
  (<*>) (Successs x) (Successs y) = Successs (x y)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    oneof [return $ Failuree e, return $ Successs a]

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

type SAP = (Sum Integer, Any, Product Integer)
testValidationTypes :: Validation SAP SAP
testValidationTypes = undefined

testValidation :: IO ()
testValidation = quickBatch $ applicative testValidationTypes

-- Chapter Exercises

pureL a = pure a :: [Integer]
applyL f a = (<*>) f a :: [Integer]
-- pureL 8
-- applyL (+1) 7

pureIO a = pure a :: IO Integer
applyIO f a = (<*>) f a :: IO Integer
-- pureIO 8
-- applyIO (pure (+1)) (pure 7)

pureT a = pure a :: (Sum Integer, Integer)
applyT f a = (<*>) f a :: (Sum Integer, Integer)
-- pureT 8
-- applyT (pure (+1)) (pure 8)

pureF a = pure a :: (Integer -> Integer)
applyF f a = (<*>) f a :: (Integer -> Integer)
-- pureF 8
-- applyF (pure (+1)) (pure 8)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair x y) (Pair u v) = Pair (x u) (y v)

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

testPairTypes :: Pair SCI
testPairTypes = undefined

testPair :: IO ()
testPair = quickBatch $ applicative testPairTypes

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two x y) (Two u v) = Two (x <> u) (y v)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

type SPS = (Sum Int, Product Int, Sum Int)
testTwoTypes :: Two SPS SPS
testTwoTypes = undefined

testTwo :: IO ()
testTwo = quickBatch $ applicative testTwoTypes

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three x y z) (Three u v w) = Three (x <> u) (y <> v) (z w)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

testThreeTypes :: Three SPS SPS SPS
testThreeTypes = undefined

testThree :: IO ()
testThree = quickBatch $ applicative testThreeTypes

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x y z) (Three' u v w) = Three' (x <> u) (y v) (z w)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

testThree'Types :: Three' SPS SPS
testThree'Types = undefined

testThree' :: IO ()
testThree' = quickBatch $ applicative testThree'Types

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four x y z q) (Four u v w p) = Four (x <> u) (y <> v) (z <> w) (q p)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

testFourTypes :: Four SPS SPS SPS SPS
testFourTypes = undefined

testFour :: IO ()
testFour = quickBatch $ applicative testFourTypes

data Four' a b = Four' a a b b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z q) = Four' x y (f z) (f q)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty x x
  (<*>) (Four' x y z q) (Four' u v w p) = Four' (x <> u) (y <> v) (z w) (q p)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

testFour'Types :: Four' SPS SPS
testFour'Types = undefined

testFour' :: IO ()
testFour' = quickBatch $ applicative testFour'Types

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

combinations = combos stops vowels stops
