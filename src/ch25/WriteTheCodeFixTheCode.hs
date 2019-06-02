module WriteTheCodeFixTheCode where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- Write the code

newtype Identity a = Identity a

rDec :: Num a => Reader a a
rDec = reader $ subtract 1

rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "Hi: " <> show a
    return $ a + 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
    putStrLn $ "Hi: " <> show a
    return $ (show a, a + 1)

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "Say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE PLS"
        Just e ->
            putStrLn ("Good, was very excite " ++ e)
