{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe
import Prelude hiding (takeWhile, dropWhile, tail, null)

import Address
import Block
import Transaction
import Persistence
import Network.Connection
import Network.Message

data NodeInfo = NodeInfo {
    _host :: HostName,
    _port :: ServiceName,
    _addr :: Address
} deriving (Show, Read, Eq)

data NodeState = NodeState {
    _chain :: MVar Blockchain,
    _pool  :: MVar TransactionPool,
    _peers :: MVar [Socket]
}

-- | Initiate a new Node Environment, for the first time Node is live
-- 
-- A new Address is generated for new Node, and save to lmdb.
initNode :: IO ()
initNode = do
    (txn, ref) <- open_lmdb "#"
    addr       <- new_addr
    put txn ref ("node_addr", hexAddr addr)
    put txn ref ("node_keys", pack . show $ keyPair addr)
    commit_txn txn
    print $ append "Node is registered, node_addr: " (hexAddr addr)

-- | Return the hex-version PublicKey of Node    
getPublicAddress db = do
    addr   <- Persistence.find db "#" "node_addr"
    return $ fromJust addr    

sycn_chain :: Socket -> Blockchain -> IO ()    
sycn_chain sock bc = do
    let msg = append "sync_chain:" (pack $ show bc)
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024
    print res    

-- | Go live a node, with empty chain, peers and txn_pool
go_live p2p_port = do
    db         <- start_lmdb
    sock       <- listenOn p2p_port
    blockchain <- newEmptyMVar
    txn_pool   <- newMVar mempty
    peers      <- newMVar mempty
    -- Initiate Node State
    let state  = NodeState {
        _chain = blockchain,
        _pool  = txn_pool,
        _peers = peers
    }
    forkIO $ listen_ sock (_peers state)
    forkIO $ conn_handle state
    return state    

-- | Handle Connections
conn_handle :: NodeState -> IO ()
conn_handle st = do
    threadDelay 2000000
    conn <- tryReadMVar (_peers st)
    forM_ (fromJust conn) $ \s -> req_handle s st
    conn_handle st

-- | Hanle incoming requests    
req_handle :: Socket -> NodeState -> IO ()    
req_handle sock st = do
    msg <- recv sock 1024
    unless (null msg) $ do
        case (takeWhile (/=':') msg) of
            "close"        -> do
                sendAll sock "Disconnected"
                close sock
            "blocks"       -> do
                chain <- tryReadMVar (_chain st)
                sendAll sock (pack $ show chain)            
            "transactions" -> do
                txns  <- tryReadMVar (_pool st)
                sendAll sock (pack $ show txns)
            "sync_chain"   -> do
                let recv_chain = read (unpack . tail $ dropWhile (/=':') msg) :: Blockchain
                chain <- tryReadMVar (_chain st)
                up_chain <- replace_chain recv_chain $ fromJust chain
                sendAll sock $ pack $ show up_chain
            "add_block"    -> do
                mined <- mineBlock genesis_block (tail $ dropWhile (/=':') msg) 0
                chain <- tryReadMVar (_chain st)
                let up_chain = Node mined (fromJust chain)
                conn  <- tryReadMVar (_peers st)
                forM_ (fromJust conn) $ \p -> sycn_chain p up_chain
            "Acknowledged" -> return ()  
            m -> sendAll sock "Acknowledged"
