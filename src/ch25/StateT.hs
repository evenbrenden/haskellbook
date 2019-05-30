{-# LANGUAGE InstanceSigs #-}

module StateT where

import Control.Monad
import Control.Monad.Trans.Class

newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where

    -- fmap :: (a -> b) -> StateT sma -> StateT smb
    fmap f (StateT sma) =
        StateT $ \s -> (fmap (\(a, s') -> (f a, s')) (sma s))

instance (Monad m) => Applicative (StateT s m) where

    pure a =
        StateT $ \s -> pure (a, s)

    -- (<*>) :: (StateT s m ab)
    --       -> (StateT s m a)
    --       -> (StateT s m b)
    (StateT smab) <*> (StateT sma) =
        StateT $ \s -> do
            -- (a -> b, s)
            abs <- smab s
            -- (a, s)
            as <- sma s
            -- (b, s)
            let bs = ((fst abs) (fst as), s)
            -- m (b, s)
            return $ bs

instance (Monad m) => Monad (StateT s m) where

    return = pure

    -- (>>=) :: StateT s m a
    --       -> (a -> StateT s m b)
    --       -> StateT s m b
    (StateT sma) >>= f =
        StateT $ \s -> do
            -- (a, s)
            as <- sma s
            -- a
            let a = fst as
            -- m (b, s)
            runStateT (f a) s

instance MonadTrans (StateT s) where
    -- lift :: (Monad m) => m a -> StateT s m a
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)
