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
blockchain = chain2
-- p2p_port = 4748
-- blockchain = chain2
-- p2p_port = 4749
-- blockchain = chain3

-- Peer info: list of (host, port)
peers = []

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

-- This is the REPL interface
peer_handle :: [Socket] -> IO ()
peer_handle socks = runInputT defaultSettings loopCmd where
    loopCmd :: InputT IO ()
    loopCmd = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "quit\r" -> return ()
            Just "sync\r" -> forM_ socks $ \sock -> do
                liftIO $ sycnChain sock blockchain
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

sycnChain sock bc = do
    let msg = C.append "sync_chain:" (C.pack $ show bc)
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
    ps <- mapM connect_ peers -- connect to peers
    forkOS $ do
        listen sock 4                              -- set a max of 4 queued connections
        print $ "Server is listening on port: " ++ (show p2p_port)
        threadDelay 2056
        listen_ sock ps                            -- listen on bound socket, keep an eye on peers
    threadDelay 4096
    peer_handle ps

listen_ :: Socket -> [Socket] -> IO ()
listen_ sock socks = do
    conn <- accept sock     -- accept an incoming connection
    forkIO $ do
        sendAll (fst conn) $ C.append "Connection established from port: " (C.pack $ show p2p_port)
        threadDelay 4000000
        sendAll (fst conn) $ C.append "sync_chain:" (C.pack $ show blockchain)-- send the ledger
        conn_handle (fst conn, socks)    -- and handle it
    listen_ sock socks           -- repeat
 
conn_handle :: (Socket, [Socket]) -> IO ()
conn_handle (sock, ps) = do
    threadDelay 4096
    msg <- recv sock 1024
    print $ C.append "Received msg: " msg
    req_handle (sock, ps) msg

req_handle :: (Socket, [Socket]) -> C.ByteString -> IO ()    
req_handle (sock, ps) msg
    | msg == "blocks" = do
        sendAll sock $ C.pack $ show chain3
        conn_handle (sock, ps)
    | C.takeWhile (/=':') msg == "sync_chain" = do
        let recv_chain = read (C.unpack . C.tail $ C.dropWhile (/=':') msg) :: Blockchain
        up_chain <- replace_chain recv_chain blockchain
        sendAll sock (C.pack $ show up_chain)
        conn_handle (sock, ps)
    | C.takeWhile (/=':') msg == "add_block" = do
        mined <- mineBlock genesis_block (C.tail $ C.dropWhile (/=':') msg)
        let up_chain = Node mined blockchain
        print $ C.pack $ show up_chain
        forM_ ps $ \p -> liftIO $ sycnChain p up_chain
        conn_handle (sock, ps)
    | otherwise = do
        sendAll sock "Disconnected"
        close sock