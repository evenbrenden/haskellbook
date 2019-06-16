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
    let mapd = Map.fromList $ zipWith (,) inis inis
    handles <- mapM ((flip openFile) ReadMode) mapd
    contents <- mapM hGetContents handles
    let configs = fmap justGetTheConfig contents
    print configs
    mapM_ hClose handles
