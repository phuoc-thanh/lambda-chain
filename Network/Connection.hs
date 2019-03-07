{-# LANGUAGE OverloadedStrings #-}

module Network.Connection (
    Socket,
    SockAddr,
    Peer(..),
    sendAll,
    recv,
    close,
    accept,
    listenOn,
    listen_,
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


data Peer = Peer {
    source :: Socket,
    pstate :: Int
} deriving (Eq, Show)

-- | Connect with default settings of Network.Socket
connect_ :: (HostName, ServiceName) -> IO Socket
connect_ (host, port) = do 
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    threadDelay 1024
    msg <- recv sock 256 -- receive welcome msg
    print msg
    return sock

-- | Listen on a bound socket
listenOn :: PortNumber -> IO Socket
listenOn p2p_port = do
    sock <- socket AF_INET Stream 0                -- create socket
    setSocketOption sock ReuseAddr 1               -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet p2p_port iNADDR_ANY)   -- listen on TCP p2p_port as config.
    listen sock 32                                 -- set a max of 32 queued connections
    print $ "Lambda-client is now listening on port: " ++ (show p2p_port)
    threadDelay 4096
    return sock

-- | Listen n Accept incoming connections
listen_ :: Socket -> MVar [Peer] -> IO ()
listen_ sock peers = do
    conn   <- accept sock
    print  $ "A new connection is established. Sock_addr: " ++ (show $ snd conn)
    -- send chain query msg, then modify list of peers (MVar peers)
    sendAll (fst conn) "chain?"
    state <- recv (fst conn) 1024
    let newPeer = Peer (fst conn) state
    modifyMVarMasked_ peers $ \lst -> return $ newPeer:lst
    listen_ sock peers

-- | Send a message on specified socket, then try receive 1024 bytes of response
sendReq :: Socket -> ByteString -> IO ()
sendReq sock msg = do
    -- this is because of windows 7 ghci put "\r" character to end the string
    -- let msg = C.init raw
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024
    print res

-- | Send a message to whole network (peers)    
sendNetwork :: [Peer] -> ByteString -> IO ()
sendNetwork peers msg = do
    forM_ peers $ \peer -> sendAll (source peer) msg       