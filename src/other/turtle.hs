{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import Data.Maybe

main = sh $ do
    let filename = "foo.txt"
    output filename ("123" <|> "456")
    line <- grep "456" $ input filename
    let text = lineToText line
    let reversed = textToLine $ T.reverse text
    let output = fromJust reversed
    liftIO (echo output)
