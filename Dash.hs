{-# LANGUAGE OverloadedStrings #-}

module Dash where

import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as M
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline

import Network.Connection
import Transaction
import Block


blockchain = chain2

txn_pool :: TransactionPool
txn_pool = []

-- Peer info: list of peer (host, port)
peers = [("127.0.0.1", "4748")]

------------------------------------------------------------------------------------------

-- As a Sender
-- Lambda-client has an interactive command line interface, allow user broadcast data to peers

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
-- Bind socket as specified port and listen, send data automatically to requesters
 
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
    | msg == "transactions" = do
        sendAll sock . C.pack $ show txn_pool
        conn_handle ((sock, sock_addr), ps)
    | C.takeWhile (/=':') msg == "sync_chain" = do
        let recv_chain = read (C.unpack . C.tail $ C.dropWhile (/=':') msg) :: Blockchain
        up_chain <- replace_chain recv_chain blockchain
        sendAll sock (C.pack $ show up_chain)
        conn_handle ((sock, sock_addr), ps)
    | C.takeWhile (/=':') msg == "add_block" = do
        mined <- mineBlock genesis_block (C.tail $ C.dropWhile (/=':') msg) 0
        let up_chain = Node mined blockchain
        print $ C.pack $ show up_chain
        socks <- tryReadMVar ps
        forM_ (fromJust socks) $ \p -> sycnChain p up_chain
        conn_handle ((sock, sock_addr), ps)
    | otherwise = do
        sendAll sock $ "Acknowledged"
        conn_handle ((sock, sock_addr), ps)