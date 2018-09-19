{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent (MVar, newMVar, newEmptyMVar, forkIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe

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

data NodeEnv = NodeEnv {
    node_info  :: NodeInfo,
    node_state :: NodeState
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

-- | Go live a node, with empty chain, peers and txn_pool
go_live p2p_port = do
    db <- start_lmdb
    sock <- listenOn p2p_port
    blockchain <- newEmptyMVar
    txn_pool <- newMVar mempty
    peers <- newMVar mempty
    let state = NodeState {
        _chain = blockchain,
        _pool = txn_pool,
        _peers = peers
    }
    forkIO $ do
        listen_ sock (_peers state)
    print "ready"

-- | Return the hex-version PublicKey of Node    
getPublicAddress db = do
    addr   <- Persistence.find db "#" "node_addr"
    return $ fromJust addr