{-# LANGUAGE OverloadedStrings #-}

module Network where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline

import DataType


-- Node info
-- Pick one of these following, as we will simulate 3 peers
p2p_port = 4747
blockchain = Genesis genesis_block
-- p2p_port = 4748
-- blockchain = chain2
-- p2p_port = 4749
-- blockchain = chain3

-- Peer info: list of (host, port)
peers = [("127.0.0.1", "4748")]

------------------------------------------------------------------------------------------

-- As a Peer
-- This part will be an interactive command line
-- Allow user send command to peers

-- Override the connect function with default settings
connect_ :: (HostName, ServiceName) -> IO Socket
connect_ (host, port) = do 
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    threadDelay 1024
    msg <- recv sock 256 -- receive welcome msg
    print $ msg
    return $ sock

peers_connect :: IO [Socket]
peers_connect = mapM connect_ peers

-- This is the REPL interface
peer_handle :: [Socket] -> IO ()
peer_handle socks = runInputT defaultSettings loopCmd where
    loopCmd :: InputT IO ()
    loopCmd = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "quit\r" -> return ()
            Just input -> forM_ socks $ \sock -> do
                liftIO $ sendReq sock (C.pack input)
        loopCmd

sendReq :: Socket -> C.ByteString -> IO ()
sendReq sock raw = do
    let msg = C.init raw -- this is because of windows ghci put "\r" character to end the string
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024 -- receive blocks msg
    print $ res -- print msg



------------------------------------------------------------------------------------------

-- As a Node
-- Bind socket as specified port and listen
-- Just like a host/provider/server
-- This part act as something automation

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet p2p_port iNADDR_ANY)   -- listen on TCP p2p_port as config.
    forkOS $ do
        listen sock 4                              -- set a max of 4 queued connections
        print $ "Server is listening on port: " ++ (show p2p_port)
        listen_ sock                                -- unimplemented
    threadDelay 4096
    ps <- peers_connect
    peer_handle ps

listen_ :: Socket -> IO ()
listen_ sock = do
    conn <- accept sock     -- accept an incoming connection
    forkIO $ do
        sendAll (fst conn) $ C.append "Connection established from port: " (C.pack $ show p2p_port)
        threadDelay 128
        -- sendAll (fst conn) $ C.pack $ show blockchain -- send the ledger
        conn_handle conn    -- and handle it
    listen_ sock            -- repeat
 
conn_handle :: (Socket, SockAddr) -> IO ()
conn_handle (sock, sock_addr) = do
    threadDelay 4096
    msg <- recv sock 1024
    print $ C.append "Received msg: " msg
    req_handle (sock, sock_addr) msg

req_handle :: (Socket, SockAddr) -> C.ByteString -> IO ()    
req_handle (sock, sock_addr) msg
    | msg == "blocks" = do
        sendAll sock $ C.pack $ show chain3
        conn_handle (sock, sock_addr)
    | C.takeWhile (/=':') msg == "sync_chain" = do
        -- new_chain <- C.tail $ C.dropWhile (/=':') msg
        -- replace_chain new_chain blockchain
        conn_handle (sock, sock_addr)       
    | C.takeWhile (/=':') msg == "add_block" = do
        mined <- mineBlock genesis_block (C.tail $ C.dropWhile (/=':') msg)
        sendAll sock $ C.pack $ show mined
        conn_handle (sock, sock_addr)
    | otherwise = do
        sendAll sock "Disconnected"
        close sock