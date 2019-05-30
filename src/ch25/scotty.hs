{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types
    (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
    hiding (get)
import Control.Monad.IO.Class

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        let hello = putStrLn "hello"
        -- (ActionT
        --     . (ExceptT . liftM Right)
        --     . (ReaderT . const)
        --     . \m -> StateT (\s -> do
        --         a <- m
        --         return (a, s))
        --     ) hello
        liftIO hello
        html $ mconcat ["<h1>Scotty, ", beam, " me up</h1>"]

