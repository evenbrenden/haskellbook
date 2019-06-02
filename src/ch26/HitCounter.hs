{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
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

        -- Fetch config from the ReaderT context.
        config <- lift ask

        -- Prepend prefix from command line argument to key.
        unprefixed <- param "key"
        let key' = mappend (prefix config) unprefixed

        -- Run IO to read the reference, apply the bump and get updated copies.
        let reference = counts config
        let counts' = readIORef $ reference
        let bumped = bumpBoomp key' <$> counts'
        (updatedMap, updatedCount) <- liftIO bumped

        -- Finally, run IO to write the updated copy of the map back to IORef.
        liftIO $ writeIORef reference updatedMap

        html $
            mconcat [ "<h1>", key', " has been visited ", TL.pack $ show updatedCount, " times.</h1>" ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR m = runReaderT m config
    scottyT 3000 runR app
