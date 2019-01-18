{-# LANGUAGE OverloadedStrings #-}

module OpeningANetworkSocket where

-- https://wiki.haskell.org/Implement_a_chat_server

-- test by running 'telnet localhost 4242'

import Network.Socket
import qualified Network.Socket.ByteString as N (send)

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    N.send sock "Hello!\n"
    close sock

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 4242 iNADDR_ANY
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock
