{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
    Config {
      counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp key map = (updatedMap, updatedCount)
    where
        updatedMap = M.insertWith (+) key 1 map
        updatedCount = fromMaybe 0 $ M.lookup key updatedMap

app :: Scotty ()
app =
    get "/:key" $ do

        unprefixed <- param "key"

        -- Fetch config from the ReaderT context.
        config <- lift ask

        -- Prepend prefix to key.
        let key' = mappend (prefix config) unprefixed

        -- Get the IORef from IO context...
        counts' <- lift . lift $ readIORef $ counts config

        -- ...make a copy with an updated count..
        let bumped = bumpBoomp key' counts'

        -- ...and write that back to IORef.
        _ <- lift . lift $ writeIORef (counts config) (fst bumped)

        let newInteger = snd bumped

        html $
            mconcat [ "<h1>Halloisen, ", key', " count is ", TL.pack $ show newInteger, "</h1>" ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR m = runReaderT m config
    scottyT 3000 runR app
