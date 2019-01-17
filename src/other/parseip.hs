module ParseIP where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char
import System.IO

parseIP :: String -> Maybe (Int, Int, Int, Int)
parseIP s = case readP_to_S ip s of
    [] -> Nothing
    xs -> Just (fst . last $ xs)

ip :: ReadP (Int, Int, Int, Int)
ip = do
    octet1 <- subnet
    char '.'
    octet2 <- subnet
    char '.'
    octet3 <- subnet
    char '.'
    octet4 <- subnet
    return $ (octet1, octet2, octet3, octet4)

subnet :: ReadP Int
subnet = do
    text <- many1 $ satisfy isDigit
    let number = read text
    if number > 255 then
        pfail
    else
        return number

main = do
    text <- getLine
    print . parseIP $ text
    main
