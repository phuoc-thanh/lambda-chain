{-# LANGUAGE OverloadedStrings #-}

module Connection where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

import DataType

-- Client side
connect_ :: HostName -> ServiceName -> IO Socket
connect_ host port = do 
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    return sock

requestBlock :: IO ()
requestBlock = do
    sock <- connect_ "127.0.0.1" "4747"
    hello <- recv sock 256 -- receive first msg
    print $ hello -- show hello msg
    sendAll sock "blocks"
    threadDelay 2048
    msg <- recv sock 1024 -- receive blocks msg
    print $ msg -- show blockchain


-- Server side    
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4747 iNADDR_ANY)   -- listen on TCP port 4747.
    listen sock 2                              -- set a max of 2 queued connections
    print "Server is listening on port 4747..."
    doLoop sock                                -- unimplemented    

doLoop :: Socket -> IO ()
doLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    forkIO $ runConn conn            -- run our server's logic
    doLoop sock           -- repeat
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello, this is a mess from server!\n"
    threadDelay 4096
    msg <- recv sock 1024
    if (msg == "blocks") then send sock $ C.pack $ show chain3 else send sock $ "Nothing"
    close sock