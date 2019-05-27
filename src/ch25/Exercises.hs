{-# LANGUAGE InstanceSigs #-}

module Exercises where

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g)
    => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

-- GOTCHA! Exercise Time

instance (Applicative f, Applicative g)
    => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure =
        Compose . pure . pure
    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose fgab) <*> (Compose fga) =
        Compose $ (fmap (<*>) fgab) <*> fga

-- How I got there using an example:
--
-- [Just (+1)] <*> [Just 1] = [Just 2]
-- fgab            fga        fgb
--
-- Going one level down, fgab <*> fga is:
--
-- (Just (+1)) (Just 1) != Just 2 (error)
-- gab         ga          gb
--
-- What we want is one more level of application:
--
-- (Just (+1)) <*> (Just 1) = Just 2
-- gab             ga         gb
--
-- So we make the <*> part of gab in order to smuggle it in on the top level:
--
-- ((Just (+1)) <*>) (Just 1) = Just 2
-- gab_app           ga         gb
--
-- Going on level up:
--
-- [((Just (+1)) <*>)] <*> [Just 1] = [Just 2]
-- fgab_app                fga        fgb
--
-- Smuggle the <*> using fmap and voila:
--
-- (fmap (<*>) [Just (+1)]) <*> [Just 1] = [Just 2]
--             fgab             fga        fgb

-- Compose Instances

instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
    foldMap f (Compose fga) =
        (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
    traverse f (Compose fga) =
        fmap Compose $ (traverse . traverse) f fga

-- And now for something completely different SKIPPED
