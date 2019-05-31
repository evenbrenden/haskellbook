module ChapterExercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class

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
