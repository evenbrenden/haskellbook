module IP where

import Control.Applicative
import Text.Trifecta
import Data.Word
import Data.Bits
import Numeric (showHex)

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

ipv4 :: Parser IPv4
ipv4 = do
    octet4 <- subnet
    char '.'
    octet3 <- subnet
    char '.'
    octet2 <- subnet
    char '.'
    octet1 <- subnet
    eof
    return $ IPv4 (octetsToWord32 octet4 octet3 octet2 octet1)

subnet :: Parser Int
-- https://stackoverflow.com/questions/36142078/how-to-signal-failure-in-trifecta-parser
subnet = (<|> unexpected "Number does not fit an octet") $ try $ do
    number <- decimal
    if number <= 255 then
        return $ fromIntegral number
    else
        empty

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
    number <- hexadecimal -- Needs an 'x' or 'X' prefix...
    if number <= 65536 then
        return $ fromIntegral number
    else
        empty

-- ...so we need this lame preformatting
addX :: String -> String
addX xs = go xs "X"
    where
        go [] ys = ys
        go (x:xs) ys =
            if x == ':' then
                go xs (ys ++ ":X")
            else
                go xs (ys ++ [x])

colonCount :: String -> Int
colonCount = length . filter (\x -> x == ':')

fillInZeros :: String -> String
fillInZeros xs = go xs []
    where
        go [] ys = ys
        go (x1:[]) ys = ys ++ [x1]
        go (x1:x2:xs) ys =
            if ([x1] ++ [x2] == "::") then
                go xs (ys ++ zeros ++ ":")
            else
                go (x2 : xs) (ys ++ [x1])
        zeros = concat $ take (numMissingZeros xs) $ repeat ":0"
        numMissingZeros = (-) 8 . colonCount

-- Does not stop addresses with more than one "::" (though fillInZeros is likely that make that fail on # hextets)
ipv6 :: Parser IPv6
ipv6 = (<|> unexpected "Invalid IPv6 address") $ try $ do
    hextets <- some $ hextet <* optional (char ':')
    eof
    if length hextets == 8 then
        return $ IPv6
            (hextetsToWord64 (hextets !! 0) (hextets !! 1) (hextets !! 2) (hextets !! 3))
            (hextetsToWord64 (hextets !! 4) (hextets !! 5) (hextets !! 6) (hextets !! 7))
    else
        empty

pip6 = parseString ipv6 mempty . addX . fillInZeros

convert :: IPv4 -> IPv6
convert (IPv4 w32) = IPv6 0 (fromIntegral w32)
