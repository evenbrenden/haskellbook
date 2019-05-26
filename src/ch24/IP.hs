module IP where

import Control.Applicative
import Text.Trifecta
import Data.Word
import Data.Bits
import Numeric (readHex, showHex)

data IPv4 =
    IPv4 Word32
    deriving (Eq, Ord)

instance Show IPv4 where
    show (IPv4 w32) =
        show (w32 `shiftR` 24) ++
        "." ++
        show ((w32 `shiftR` 16) .&. 255) ++
        "." ++
        show ((w32 `shiftR` 8) .&. 255) ++
        "." ++
        show (w32 .&. 255)

octetsToWord32:: Int -> Int -> Int -> Int -> Word32
octetsToWord32 d c b a = fromIntegral $ (d `shiftL` 24) .|. (c `shiftL` 16) .|. (b `shiftL` 8) .|. a

octet :: Parser Int
-- https://stackoverflow.com/questions/36142078/how-to-signal-failure-in-trifecta-parser
octet = (<|> unexpected "Number does not fit an octet") $ try $ do
    number <- decimal
    if number <= 255 then
        return $ fromIntegral number
    else
        empty

ipv4 :: Parser IPv4
ipv4 = do
    octet4 <- octet
    char '.'
    octet3 <- octet
    char '.'
    octet2 <- octet
    char '.'
    octet1 <- octet
    eof
    return $ IPv4 (octetsToWord32 octet4 octet3 octet2 octet1)

pip4 = parseString ipv4 mempty

data IPv6 =
    IPv6 Word64 Word64
    deriving (Eq, Ord)

instance Show IPv6 where
    show (IPv6 msbs lsbs) =
        showHex (msbs `shiftR` 48) "" ++
        ":" ++
        showHex ((msbs `shiftR` 32) .&. 65535) "" ++
        ":" ++
        showHex ((msbs `shiftR` 16) .&. 65535) "" ++
        ":" ++
        showHex (msbs .&. 65535) "" ++
        ":" ++
        showHex (lsbs `shiftR` 48) "" ++
        ":" ++
        showHex ((lsbs `shiftR` 32) .&. 65535) "" ++
        ":" ++
        showHex ((lsbs `shiftR` 16) .&. 65535) "" ++
        ":" ++
        showHex (lsbs .&. 65535) ""

hextetsToWord64:: Int -> Int -> Int -> Int -> Word64
hextetsToWord64 d c b a = fromIntegral $ (d `shiftL` 48) .|. (c `shiftL` 32) .|. (b `shiftL` 16) .|. a

hextet :: Parser Int
hextet = (<|> unexpected "Number does not fit a hextet") $ try $ do
    digits <- some hexDigit
    let number = hexx digits
    if number <= 65536 then
        return number
    else
        empty
    where
        hexx :: String -> Int
        hexx = fst . head . readHex

mergeHextets :: [Int] -> [Int] -> [Int]
mergeHextets left right = left ++ (take numMissingZeros $ repeat 0) ++ right
    where
        numMissingZeros =
            if (length right == 0) then
                0 -- No "::" so left must have exactly 8 hextets
            else
                8 - (length left + length right)

ipv6 :: Parser IPv6
ipv6 = (<|> unexpected "Invalid IPv6 address") $ try $ do
    leftHextets <- some $ hextet <* optional (char ':')
    optional (char ':') -- "::"
    rightHextets <- many $ hextet <* optional (char ':')
    let hextets = mergeHextets leftHextets rightHextets
    eof
    if length hextets == 8 then
        return $ IPv6
            (hextetsToWord64 (hextets !! 0) (hextets !! 1) (hextets !! 2) (hextets !! 3))
            (hextetsToWord64 (hextets !! 4) (hextets !! 5) (hextets !! 6) (hextets !! 7))
    else
        empty

pip6 = parseString ipv6 mempty

convert :: IPv4 -> IPv6
convert (IPv4 w32) = IPv6 0 (fromIntegral w32)
