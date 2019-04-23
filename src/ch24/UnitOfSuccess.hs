module UnitOfSuccess where

import Text.Parser.Combinators
import Text.Trifecta

numberOnly :: Parser Integer
numberOnly = do
    number <- decimal
    eof
    return number

unitOfSuccess :: String -> IO ()
unitOfSuccess s = do
    print $ parseString numberOnly mempty s
