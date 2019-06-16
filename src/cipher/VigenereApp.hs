module Vigenere where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, hWaitForInput, stdout, stdin, stderr)
import Cipher (vigenere, unVigenere)

getLine' :: IO String
getLine' = go ""
    where
        go line = do
            input <- hGetChar stdin
            case input of
                '\n' -> return line
                _   -> go $ line ++ [input]

doCipher :: String -> String -> IO ()
doCipher key mode = do
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

doCipherWithTimeout :: String -> String -> Int -> IO ()
doCipherWithTimeout key mode timeout = do
    madeItInTime <- hWaitForInput stdin timeout
    if madeItInTime then
        doCipher key mode
    else do
        hPutStr stderr "Timed out\n"
        exitFailure

main :: IO ()
main = do
    args <- getArgs
    let [key, mode] = take 2 args
    if length args == 3 then do
        let timeout = read (args !! 2)
        doCipherWithTimeout key mode timeout
     else if length args == 2 then do
        doCipher key mode
    else do
        hPutStr stderr "Too few or too many arguments\n"
        exitFailure
