module TelefonNummer where

import Text.Trifecta

type TelefonNummer = Integer

-- No conventions for how the digits are chunked AFAIK but this should pass for an input field validator.

-- Examples:
-- +47 123 45 678
-- 12 34 56 78
-- 123456789

parseTelefonNummer :: Parser TelefonNummer
parseTelefonNummer = do
    _ <- optional $ skipMany (char ' ') >> string "+47" >> skipMany (char ' ')
    number <- some (token digit)
    if length number == 8 then
        return $ read number
    else
        fail "Number must be 8 digits"

ptn = parseString parseTelefonNummer mempty
