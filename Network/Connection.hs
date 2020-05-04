{-# LANGUAGE OverloadedStrings #-}

module Network.Connection (
    Socket,
    SockAddr,
    Node(..),
    sendAll,
    recv,
    close,
    accept,
    listenOn,
    connect_,
    sendReq,
    sendNetwork
) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (append, pack)
import Control.Concurrent
import Control.Monad (forM_)


data Node = Node {
    _sock :: Socket,
    _addr :: SockAddr,
    _noid :: ThreadId
} deriving (Show, Eq)

-- | Connect with default settings of Network.Socket
connect_ :: (HostName, ServiceName) -> IO Node
connect_ (host, port) = do 
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    node_id <- forkIO $ connect sock (addrAddress serveraddr)
    threadDelay 1024
    msg <- recv sock 256 -- receive welcome msg
    print msg
    return $ Node sock (addrAddress serveraddr) node_id

-- | Listen on a bound socket
listenOn :: PortNumber -> MVar [Node] -> IO ()
listenOn p2p_port peers = do
    sock <- socket AF_INET Stream 0                -- create socket
    setSocketOption sock ReuseAddr 1               -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet p2p_port iNADDR_ANY)   -- listen on TCP p2p_port as config.
    listen sock 8                                  -- set a max of 8 queued connections
    print $ "Lambda-client is now listening on port: " ++ (show p2p_port)
    threadDelay 4096
    listen_ sock peers                             -- Accept incoming connections

-- | Listen n Accept incoming connections
listen_ :: Socket -> MVar [Node] -> IO ()
listen_ sock peers = do
    (peer_sock, peer_addr) <- accept sock
    print $ "A new connection is established. Sock_addr: " ++ (show peer_addr)
    -- send welcome msg, then modify list of peers (MVar [Node])
    node_id <- forkIO $ sendAll peer_sock "welcome"
    modifyMVarMasked_ peers $ \lst -> return $ Node peer_sock peer_addr node_id : lst
    -- handle threads
    listen_ sock peers

-- | Send a message on specified socket, then try receive 1024 bytes of response
sendReq :: Node -> ByteString -> IO ()
sendReq node msg = do
    -- Caution: In some cases of windows 7, ghci put "\r" character to end the string
    -- let msg = C.init raw
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024
    print res
        where sock = _sock node

-- | Send a message to whole network (peers)    
sendNetwork :: [Node] -> ByteString -> IO ()
sendNetwork nodes msg = do
    forM_ nodes $ \n -> sendAll (_sock n) msg       