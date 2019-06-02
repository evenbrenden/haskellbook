module WrapItUp where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List

-- Maybe (base) and Except monads stacked
simplerExample :: MaybeT (ExceptT String []) Int
simplerExample = MaybeT $ ExceptT [Right (Just 1)]

embedded :: MaybeT
            (ExceptT String
                (ReaderT () IO))
            Int
embedded = MaybeT $ ExceptT $ ReaderT $ fmap pure (const (Right (Just 1)))
