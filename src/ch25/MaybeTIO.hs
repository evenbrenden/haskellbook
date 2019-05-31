module MaybeTIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

-- Stubbing out IO - real implementation would check that f actually exists
sizeT :: FilePath -> MaybeT IO Integer
sizeT f =
    MaybeT $ pure $ case f == "the_file_im_looking_for.txt" of
        False -> Nothing
        True -> Just 1000

sumSizes :: FilePath
     -> FilePath
     -> IO (Maybe Integer)
sumSizes f1 f2 = runMaybeT $ do
    s1 <- sizeT f1
    liftIO (putStrLn "File 1 exists")
    s2 <- sizeT f2
    liftIO (putStrLn "File 2 exists")
    return (s1 + s2)
