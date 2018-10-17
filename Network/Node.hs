{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv, Raw)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe
import Prelude hiding (takeWhile, dropWhile, take, tail, null, length, replicate)

import Address
import Block
import Crypto
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
    _chain :: MVar [Block],
    _pool  :: MVar [Transaction],
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
getPublicAddress = find' "#" "hex_addr"    

send_to :: ByteString -> Int -> IO (Maybe Transaction)
send_to recvAddr amount = do
    node_addr  <- find' "#" "node_addr"
    let addr    = read $ unpack node_addr :: Address
    transfer addr recvAddr amount

-- The mine rate of bitcoin is 10 min (600s).
-- This is just demonstration, so I set it to only 3s.
mine_rate = 3

mine_block :: [Transaction] -> IO Block
mine_block txs = do
    timestamp  <- now
    origin     <- getPublicAddress
    let txH    =  Transaction.hash_id <$> txs
    let m_root =  merkle_root txH
    prev_id    <- last_block_id
    bits       <- adjust_diff
    header     <- hash_calculate origin prev_id m_root bits 0
    return $ Block header origin (size txH) txH

-- | Calcualte block_hash, then return a BlockHeader (that holds nonce n timestamp)
hash_calculate origin prev_id m_root bits nonce = do
    timestamp  <- now
    let hashed = showBS . hash . append prev_id 
                               . append (showBS timestamp) 
                               . append m_root 
                               $ append origin (showBS nonce)
    if take bits hashed == replicate bits '0'
        then return $ BlockHeader prev_id timestamp m_root bits nonce
        else hash_calculate origin prev_id m_root bits (nonce + 1)


-- Adjust the difficulty of mining process                                
adjust_diff :: IO Int                                
adjust_diff = do
    blk <- last_block
    let b = bits (blockHeader blk)
    let t = timestamp (blockHeader blk)
    if t + mine_rate > t then return $ b + 1 else return $ b - 1


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
            Raw m      -> print m
