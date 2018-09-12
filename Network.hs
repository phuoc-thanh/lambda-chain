{-# LANGUAGE OverloadedStrings #-}

module Network where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as M
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

-- Peer info: list of peer (host, port)
peers = [("127.0.0.1", "4748")]

------------------------------------------------------------------------------------------

-- As a Sender
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

-- The REPL interface
peer_handle :: MVar [Socket] -> IO ()
peer_handle mv = runInputT defaultSettings loopCmd where
    loopCmd :: InputT IO ()
    loopCmd = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "connect" -> liftIO $ do 
                pSocks <- mapM connect_ peers
                tryPutMVar mv pSocks
                -- print $ (show b) ++ "Peer Sockets are connected" -- To be verify later
                return ()
            Just input -> liftIO $ do
                socks <- tryReadMVar mv
                sendNetwork (fromJust socks) (C.pack input)
        loopCmd

sendReq :: Socket -> ByteString -> IO ()
sendReq sock msg = do
    -- let msg = C.init raw -- this is because of windows ghci put "\r" character to end the string
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024 -- receive blocks msg
    print $ res -- print msg

sendNetwork :: [Socket] -> ByteString -> IO ()
sendNetwork socks msg = do
    forM_ socks $ \sock -> sendReq sock msg    

sycnChain :: Socket -> Blockchain -> IO ()    
sycnChain sock bc = do
    let msg = C.append "sync_chain:" (C.pack $ show bc)
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024 -- receive blocks msg
    print $ res -- print msg

------------------------------------------------------------------------------------------

-- As a Receiver
-- Bind socket as specified port and listen
-- Just like a host/provider/server
-- This part act as something automation

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet p2p_port iNADDR_ANY)   -- listen on TCP p2p_port as config.
    ps <- newEmptyMVar                             -- init an empty MVar for Sockets
    forkOS $ do
        listen sock 4                              -- set a max of 4 queued connections
        print $ "Lambda-client is now listening on port: " ++ (show p2p_port)
        threadDelay 2056
        listen_ sock ps                            -- listen on bound socket, keep an eye on peers
    threadDelay 4096
    peer_handle ps

listen_ :: Socket -> MVar [Socket] -> IO ()
listen_ sock socks = do
    conn <- accept sock     -- accept an incoming connection
    print $ "A new connection is established. Sock_addr: " ++ (show $ snd conn)
    forkIO $ do
        sendAll (fst conn) $ C.append "You are connected to: " (C.pack $ show sock)-- send the ledger
        -- modifyMVarMasked_ socks $ \lst -> return ((fst conn):lst)
        conn_handle (conn, socks)    -- and handle it
    listen_ sock socks           -- repeat
 
conn_handle :: ((Socket, SockAddr), MVar [Socket]) -> IO ()
conn_handle ((sock, sock_addr), socks) = do
    threadDelay 4096
    msg <- recv sock 1024
    print $ C.append "Received msg from " $ C.append (C.pack $ show sock_addr) $ C.append ": " msg
    req_handle ((sock, sock_addr), socks) msg

req_handle :: ((Socket, SockAddr), MVar [Socket]) -> ByteString -> IO ()    
req_handle ((sock, sock_addr), ps) msg
    | msg == "close" = do
        sendAll sock "Disconnected"
        close sock
    | msg == "blocks" = do
        sendAll sock $ C.pack $ show chain3
        conn_handle ((sock, sock_addr), ps)
    | C.takeWhile (/=':') msg == "sync_chain" = do
        let recv_chain = read (C.unpack . C.tail $ C.dropWhile (/=':') msg) :: Blockchain
        up_chain <- replace_chain recv_chain blockchain
        sendAll sock (C.pack $ show up_chain)
        conn_handle ((sock, sock_addr), ps)
    | C.takeWhile (/=':') msg == "add_block" = do
        mined <- mineBlock genesis_block (C.tail $ C.dropWhile (/=':') msg)
        let up_chain = Node mined blockchain
        print $ C.pack $ show up_chain
        socks <- tryReadMVar ps
        forM_ (fromJust socks) $ \p -> sycnChain p up_chain
        conn_handle ((sock, sock_addr), ps)
    | otherwise = do
        sendAll sock $ "Acknowledged"
        conn_handle ((sock, sock_addr), ps)