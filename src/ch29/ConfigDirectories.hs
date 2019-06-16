module ConfigDirectories where

import System.Directory (listDirectory)
import System.IO
import Data.List (isSuffixOf, zip)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Text.Trifecta
import ParsingConfigurationFiles (maybeSuccess, parseConfig)

parseTheContents = parseByteString parseConfig mempty . pack
justGetTheConfig = fromJust . maybeSuccess . parseTheContents

main :: IO ()
main = do
    files <- listDirectory "."
    let inis = filter (isSuffixOf ".ini") files
    handles <- mapM ((flip openFile) ReadMode) inis
    contents <- mapM hGetContents handles
    let parses = justGetTheConfig <$> contents
    let mapped = Map.fromList $ zip inis parses
    print mapped
    mapM_ hClose handles
