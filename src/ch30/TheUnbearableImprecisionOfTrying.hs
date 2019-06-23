module TheUnbearableImprecisionOfTrying where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomIO)

canICatch :: Exception e
          => e
          -> IO (Either SomeException ())
canICatch e =
    try $ throwIO e

randomException :: IO ()
randomException = do
    fiftyFifty <- randomIO :: IO Bool
    case fiftyFifty of
        True -> throwIO DivideByZero
        False -> throwIO StackOverflow

handleArith :: ArithException -> IO ()
handleArith DivideByZero = putStr "Division by zero, but..."
handleArith e = throwIO e

handleAsync :: AsyncException -> IO ()
handleAsync StackOverflow = putStr "Stack overflow, but..."
handleAsync e = throwIO e

main :: IO ()
main = forever $ do
    _ <- randomException `catches` [ Handler handleArith,
                                     Handler handleAsync ]
    putStrLn "...live to loop another day!"
    threadDelay (1 * 1000000)
