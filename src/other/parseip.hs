module ParseIP where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char

parseIP :: String -> Maybe (Int, Int, Int, Int)
parseIP s = case readP_to_S ip s of
    [] -> Nothing
    [(t,_)] -> Just t

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
    num <- numbers 1 <|> numbers 2 <|> numbers 3
    if num > 255 then
        pfail
    else
        return num

numbers :: Int -> ReadP Int
numbers digits = do
    parse <- count digits digit
    return $ read parse

digit :: ReadP Char
digit =
    satisfy isDigit
