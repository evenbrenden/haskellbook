{-# LANGUAGE TemplateHaskell #-}

-- Need hg installed for this to compile

module Hgrev where

import           Data.Monoid          ((<>))
import           Data.Text            (Text, pack)
import           Development.HgRev.TH (defFormat, hgRevStateTH, jsonFormat)
import           Options.Applicative  (Parser, ParserInfo, execParser, fullDesc,
                                       help, helper, info, infoOption, long,
                                       progDesc, short)

main :: IO ()
main = execParser parserInfo >> return ()

verSwitch :: Parser (a -> a)
verSwitch =
    infoOption ("HG rev: ")
    $  long "version"
    <> short 'v'
    <> help "Display version information"

jsonSwitch :: Parser (a -> a)
jsonSwitch =
    infoOption "string"
    $  long "json"
    <> short 'J'
    <> help "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo = info (helper <*> verSwitch <* jsonSwitch) fullDesc
