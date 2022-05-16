module Chapter20Exercises where

import           Data.Monoid

newtype Constant a b = Constant b
  deriving Show

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x
  foldr f z (Constant x) = f x z

data Two a b = Two a b
  deriving Show

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y
  foldr f z (Two _ y) = f y z

data Three a b c = Three a b c
  deriving Show

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z
  foldr f z0 (Three _ _ z) = f z z0

data Three' a b = Three' a b b
  deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z
  foldr f z0 (Three' _ y z) = f y (f z z0)

data Four' a b = Four' a a b b
  deriving Show

instance Foldable (Four' a) where
  foldMap f (Four' _ _ z w) = f z <> f w
  foldr f z0 (Four' _ _ z w) = f z (f w z0)

filterF
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap $ \x -> if f x then pure x else mempty
