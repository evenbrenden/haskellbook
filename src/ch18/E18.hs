module E18 where

import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The answer is the exercise

bind :: Monad m => (a -> m b) -> m a -> m b
bind amb ma = join $ fmap amb ma

-- Short exercise: Either Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  Second f <*> Second y = Second $ f y
  Second x <*> First  y = First y
  First  x <*> _ = First x

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) f = (First a)
  (>>=) b f = case f <$> b of
    Second (First x) -> First x
    Second (Second x) -> Second x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

type III = (Integer, Integer, Integer)
testSumTypes :: Sum III III
testSumTypes = undefined

testSum = do
    quickBatch $ functor testSumTypes
    quickBatch $ applicative testSumTypes
    quickBatch $ monad testSumTypes

-- Chapter Exercises

-- Nope monad

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) = undefined

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

testNope = do
    let trigger :: Nope (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ monad trigger

-- PhhhbbtttEither monad

data PhhhbbtttEither b a =
    Leftt a
  | Rightt b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Leftt a) = Leftt (f a)
  fmap _ (Rightt b) = Rightt b

instance Applicative (PhhhbbtttEither a) where
  pure = Leftt
  (<*>) = undefined

instance Monad (PhhhbbtttEither a) where
  return = pure
  (>>=) (Rightt b) f = (Rightt b)
  (>>=) a f = case f <$> a of
    Leftt (Leftt x) -> Leftt x
    Leftt (Rightt x) -> Rightt x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Leftt b, return $ Rightt a]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

testPhhhbbtttEither = do
    let trigger :: PhhhbbtttEither (Int, String, Int) (String, Int, String)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ monad trigger

-- Identity monad

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) = undefined

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

testIdentity = do
    let trigger :: Identity (Int, String, Char)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ monad trigger

-- List monad

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

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

instance Functor List where
  fmap x Nil = Nil
  fmap x (Cons a l) = Cons (x a) (fmap x l)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) = undefined

instance Monad List where
  return = pure
  (>>=) = flip flatMap

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toMyList <$> arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

testList = do
    let trigger :: List (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ monad trigger

-- Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition is fine, but it has to typecheck with types provided.

j :: Monad m => m (m a) -> m a
j m = m >>= id -- join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap -- liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = fmap f a <*> b -- liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>) -- apply

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh xs f = flipType $ fmap f xs

-- https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Traversable.html#sequence
flipType :: Monad m => [m a] -> m [a]
-- flipType xs = go xs (return [])
--   where
--     go :: Monad m => [m a] -> m [a] -> m [a]
--     go [] bs = bs
--     go (a:as) bs = go as (hearItSquish a bs)
--     hearItSquish :: Applicative m => m a -> m [a] -> m [a]
--     -- hearItSquish x y = (:) <$> x <*> y
--     hearItSquish = liftA2 (:)
flipType = foldr (liftA2 (:)) (return [])
