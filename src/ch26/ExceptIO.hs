module ExceptTIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except

-- Stubbing out IO - real implementation would check that f actually exists
sizeT :: FilePath -> ExceptT String IO Integer
sizeT f =
    ExceptT $ pure $ case f == "the_file_im_looking_for.txt" of
        False -> Left "No such file"
        True -> Right 1000

sumSizes :: FilePath
     -> FilePath
     -> IO (Either String Integer)
sumSizes f1 f2 = runExceptT $ do
    s1 <- sizeT f1
    liftIO (putStrLn "File 1 exists")
    s2 <- sizeT f2
    liftIO (putStrLn "File 2 exists")
    return (s1 + s2)

main :: IO ()
main = do
    totalSize <- sumSizes "the_file_im_looking_for.txt" "the_file_im_looking_for.txt" -- Try changing these
    case totalSize of
        (Left error) -> print error
        (Right size) -> print size
