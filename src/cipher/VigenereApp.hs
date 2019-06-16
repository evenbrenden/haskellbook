module Vigenere where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, stdout, stdin)
import Cipher (vigenere, unVigenere)

getLine' :: IO String
getLine' = go ""
    where
        go line = do
            input <- hGetChar stdin
            case input of
                '\n' -> return line
                _   -> go $ line ++ [input]

main :: IO ()
main = do
    [key, mode] <- getArgs
    input <- getLine'
    case mode of
        "-e" -> do
            hPutStr stdout $ vigenere input key
            hPutStr stdout "\n"
            exitSuccess
        "-d" -> do
            hPutStr stdout $ unVigenere input key
            hPutStr stdout "\n"
            exitSuccess
        _    -> do
            hPutStr stdout "Mode unknown\n"
            exitFailure
