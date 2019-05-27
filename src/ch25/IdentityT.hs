{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IdentityT where

import Control.Monad

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where

    fmap f (IdentityT fa) =
        IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where

    pure x = IdentityT (pure x)

    (IdentityT fab) <*> (IdentityT fa) =
        IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where

    return = pure

    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    (IdentityT ma) >>= (f :: (a -> IdentityT m b)) =
        -- Step 1: Figure it out
        -- let impl :: a -- Trick to force GHC to tell us what type impl has (cannot be anything, like this signature suggests)
        --     impl = join (fmap runIdentityT (fmap f ma))
        -- in undefined -- Trick to focus on impl definition without noise from not type checking with the surrounding signature
        -- Step 2: Make it work
        -- let impl :: m b
        --     impl = join (fmap runIdentityT (fmap f ma))
        -- in IdentityT impl
        -- Step 3: Refactor
        IdentityT $ ma >>= (runIdentityT . f)
