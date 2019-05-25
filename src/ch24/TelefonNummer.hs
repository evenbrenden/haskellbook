module TelefonNummer where

import Control.Applicative
import Text.Trifecta

type TelefonNummer = Integer

-- Examples:
-- +47 123 45 678
-- 12 34 56 78
-- 123456789
-- +47123456789

parseTwoDigits :: Parser String
parseTwoDigits = do
    first <- digit
    second <- digit
    return $ first : [second]

parseThreeDigits :: Parser String
parseThreeDigits = do
    firstAndSecond <- parseTwoDigits
    third <- digit
    return $ firstAndSecond <> [third]

parseTelefonNummer323 :: Parser String
parseTelefonNummer323 = do
    first <- parseThreeDigits
    _ <- space
    second <- parseTwoDigits
    _ <- space
    third <- parseThreeDigits
    _ <- eof
    return $ first <> second <> third

parseTelefonNummer2222 :: Parser String
parseTelefonNummer2222 = do
    first <- parseTwoDigits
    _ <- space
    second <- parseTwoDigits
    _ <- space
    third <- parseTwoDigits
    _ <- space
    fourth <- parseTwoDigits
    _ <- eof
    return $ first <> second <> third <> fourth

parseTelefonNummer8 :: Parser String
parseTelefonNummer8 = do
    number <- some digit
    if length number == 8 then
        return number
    else
        fail "Number must be 8 digits"

parseTelefonNummer :: Parser TelefonNummer
parseTelefonNummer = do
    _ <- optional $ string "+47"
    _ <- optional space
    -- Error messages could get weird because parseTelefonNummer8 is always the failing parser.
    number <- choice [try parseTelefonNummer323, try parseTelefonNummer2222, parseTelefonNummer8]
    return $ read number

ptn = parseString parseTelefonNummer mempty
