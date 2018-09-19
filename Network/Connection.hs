{-# LANGUAGE OverloadedStrings #-}

module Network.Connection (
    Socket,
    SockAddr,
    sendAll,
    recv,
    close,
    accept,
    listenOn,
    listen_,
    connect_
) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (append, pack)
import Control.Concurrent


-- | Connect with default settings of Network.Socket
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

-- | Listen on a bound socket
listenOn :: PortNumber -> IO Socket
listenOn p2p_port = do
    sock <- socket AF_INET Stream 0                -- create socket
    setSocketOption sock ReuseAddr 1               -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet p2p_port iNADDR_ANY)   -- listen on TCP p2p_port as config.
    listen sock 32                                 -- set a max of 4 queued connections
    print $ "Lambda-client is now listening on port: " ++ (show p2p_port)
    threadDelay 4096
    return sock

listen_ :: Socket -> MVar [Socket] -> IO ()
listen_ sock socks = do
    conn <- accept sock     -- accept an incoming connection
    print $ "A new connection is established. Sock_addr: " ++ (show $ snd conn)
    forkIO $ do
        sendAll (fst conn) $ append "You are connected to: " (pack $ show sock)
        modifyMVarMasked_ socks $ \lst -> return ((fst conn):lst)
        -- conn_handle (conn, socks)    -- and handle it
    listen_ sock socks           -- repeat