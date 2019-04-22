-- https://github.com/dmvianna/haskellbook/blob/master/src/Ch24-LearnParsers.hs
{-# LANGUAGE RankNTypes #-}

module ParsingPractice where

import Text.Parser.Combinators
import Text.Trifecta
import Data.Maybe

-- Part 1

oneOnly :: Parser ()
oneOnly = char '1' >> eof

oneTwoOnly :: Parser ()
oneTwoOnly = char '1' >> char '2' >> eof

parse123Only :: Parser () -> IO ()
parse123Only p = print $ parseString p mempty "123"

part1 = do
    parse123Only oneOnly
    parse123Only oneTwoOnly

-- Part 2

stop:: Parser a
stop = unexpected "stop"

-- https://github.com/dmvianna/haskellbook/blob/master/src/Ch24-LearnParsers.hs
type MString = forall m. CharParsing m => m String

oneString :: MString
oneString = string "1"

oneTwoString :: MString
oneTwoString = string "12"

oneTwoThreeString :: MString
oneTwoThreeString = string "123"

parse123String :: String -> IO ()
parse123String s = print $ parseString p mempty s
    where p = choice [ oneTwoThreeString, oneTwoString, oneString, stop ]

part2 = do
    parse123String "1"
    parse123String "12"
    parse123String "123"

-- Part 3

parse123Char :: String -> IO ()
parse123Char s = print $ parseString p mempty s
    where
        p = do
            char '1'
            optional $ char '2'
            optional $ char '3'
            return s

part3 = do
    parse123Char "1"
    parse123Char "12"
    parse123Char "123"
