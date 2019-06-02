{-# LANGUAGE InstanceSigs #-}

module MaybeT where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
      => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma


instance (Applicative m)
      => Applicative (MaybeT m) where

    pure x = MaybeT (pure (pure x))

    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
      => Monad (MaybeT m) where

    return = pure

    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)

instance MonadTrans MaybeT where
    lift :: (Monad m) => m a -> MaybeT m a
    lift = MaybeT . liftM Just

instance (MonadIO m)
        => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    liftIO = lift . liftIO
