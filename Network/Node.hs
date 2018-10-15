{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv, Raw)
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
    put txn ref ("hex_addr" , hexAddr addr)
    put txn ref ("node_addr", showBS addr)
    put txn ref ("node_keys", showBS $ keyPair addr)
    put txn ref ("balance"  , "50")
    commit_txn txn
    print $ append "Node is registered, node_addr: " (hexAddr addr)

-- | Return the hex-version PublicKey of Node    
getPublicAddress = do
    (txn, dbi) <- open_lmdb "#" 
    addr       <- find_ txn dbi "hex_addr"
    return $ fromJust addr    

send_to :: ByteString -> Int -> IO (Maybe Transaction)
send_to recvAddr amount = do
    (txn, dbi) <- open_lmdb "#"
    node_addr  <- find_ txn dbi "node_addr"
    let addr    = read . unpack $ fromJust node_addr :: Address
    transfer addr recvAddr amount

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
    raw <- recv sock 1024
    unless (null raw) $ do
        case (rawToMsg raw) of          
            TxnReq txn -> modifyMVarMasked_ (_pool st) $ \txns -> return $ expand_pool txn txns
            BlockReq b -> modifyMVarMasked_ (_chain st) $ \lst -> return $ b:lst
            Raw -> return ()            
