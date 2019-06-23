module TheUnbearableImprecisionOfTrying where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

canICatch :: Exception e
          => e
          -> IO (Either SomeException ())
canICatch e =
    try $ throwIO e

randomException :: IO ()
randomException = do
    i <- randomRIO (1, 10 :: Int)
    if i `elem` [1..9]
        then throwIO DivideByZero
        else throwIO StackOverflow

main :: IO ()
main = forever $ do
    let tryS :: IO ()
             -> IO (Either SomeException ())
        tryS = try
    _ <- tryS randomException
    putStrLn "Live to loop another day!"
    threadDelay (1 * 1000000)
