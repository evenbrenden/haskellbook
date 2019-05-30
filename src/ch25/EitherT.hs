module EitherT where

import Data.Either

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where

    fmap f (EitherT em) =
        EitherT $ (fmap . fmap) f em

instance (Applicative m) => Applicative (EitherT e m) where

    pure = EitherT . pure . pure

    (EitherT fem) <*> (EitherT em) =
        EitherT $ (fmap (<*>) fem) <*> em

instance (Monad m) => Monad (EitherT e m) where

    return = pure

    -- (>>=) :: EitherT e m a
    --       -> (a -> EitherT e m b)
    --       -> EitherT e m b
    (EitherT em) >>= f =
        EitherT $ do
            v <- em
            case v of
                Left e -> return $ Left e
                Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left ea) = Right ea
swapEither (Right ea) = Left ea

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT amb) =
    do
        ab <- amb
        case ab of
            Left a -> amc a
            Right b -> bmc b
