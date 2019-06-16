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
    let tups :: [(FilePath, FilePath)]
        tups = zipWith (,) inis inis
    handles <- (mapM . mapM) ((flip openFile) ReadMode) tups
    contents <- (mapM . mapM) hGetContents handles
    let parses = (fmap . fmap) justGetTheConfig contents
    let mapped = Map.fromList parses
    print mapped
    (mapM_ . mapM_) hClose handles
