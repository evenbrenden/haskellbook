{-# LANGUAGE OverloadedStrings #-}

module MoreParsing where

import Data.Aeson
import Data.Aeson.Types
import Options.Applicative

-- p = Payload "yo" "no" "me" "you" "them"
-- e = encode p
-- decode e :: Maybe Payload

data Payload = Payload {
    from :: String,
    to :: String,
    subject :: String,
    body :: String,
    offset_seconds :: String
} deriving Show

instance FromJSON Payload where
  parseJSON (Object v) =
    Payload <$> v .: "from"
            <*> v .: "to"
            <*> v .: "subject"
            <*> v .: "body"
            <*> v .: "offset_seconds"
  parseJSON v = typeMismatch "Payload" v

instance ToJSON Payload where
  toJSON p = object [
    "from" .= from p,
    "to"  .= to p,
    "subject"  .= subject p,
    "body"  .= body p,
    "offset_seconds"  .= offset_seconds p ]
