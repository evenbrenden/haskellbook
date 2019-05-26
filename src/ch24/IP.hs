module IP where

import Control.Applicative
import Text.Trifecta
import Data.Word
import Data.Bits

data IPv4 =
    IPv4 Word32
    deriving (Eq, Ord, Show)

octetsToWord32:: Int -> Int -> Int -> Int -> Word32
octetsToWord32 d c b a = fromIntegral $ (d `shiftL` 24) .|. (c `shiftL` 16) .|. (b `shiftL` 8) .|. a

ipv4 :: Parser IPv4
ipv4 = do
    octet32 <- subnet
    char '.'
    octet24 <- subnet
    char '.'
    octet16 <- subnet
    char '.'
    octet8 <- subnet
    eof
    return $ IPv4 (octetsToWord32 octet32 octet24 octet16 octet8)

subnet :: Parser Int
-- https://stackoverflow.com/questions/36142078/how-to-signal-failure-in-trifecta-parser
subnet = (<|> unexpected "Subnet number does not fit an octet") $ try $ do
    number <- decimal
    if number <= 255 then
        return $ fromIntegral number
    else
        empty

pip4 = parseString ipv4 mempty
