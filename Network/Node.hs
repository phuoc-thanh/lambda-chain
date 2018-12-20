{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv, Raw)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe
import Prelude hiding (takeWhile, dropWhile, take, tail, null, length, replicate, concat)

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
    _db        :: Lambdadb,
    _pool      :: MVar [Transaction],
    _peers     :: MVar [Socket]
}

-- | Initiate a new Node Environment, for the first time Node is live

{- A new Address will be generated for new Node, and save to lmdb.
   Node's ledger is also be reset to keep only the genesis block. -}
init_node :: IO ()
init_node = do
    reset_lmdb
    (txn, ref) <- open_lmdb "#"
    addr       <- new_addr
    put txn ref ("hex_addr" , hexAddr addr)
    put txn ref ("node_addr", showBS addr)
    put txn ref ("node_keys", showBS $ keyPair addr)
    put txn ref ("balance"  , "50")
    put txn ref ("block#1"  , "block#c8925588637c65e719681d1275d8d87c2b305744992e1e7ff6597bb5f918e9e6")
    commit_txn txn
    (txn2, db) <- open_lmdb "@"
    put txn2 db (append "block#" $ Block.hash_id genesis_header [], showBS genesis_block)
    commit_txn txn2
    print $ append "Node is registered, node_addr: " (hexAddr addr)
    where
        genesis_block  = Block genesis_header "f1rstM1n3r" 0 []
        genesis_header = BlockHeader "genesis" 1538583356613 "no-merkle-root" 4 0

-- | Return the hex-version PublicKey of Node    
getPublicAddress = find' "#" "hex_addr"    

send_to :: ByteString -> Int -> IO (Maybe Transaction)
send_to recvAddr amount = do
    node_addr  <- find' "#" "node_addr"
    let addr    = read $ unpack node_addr :: Address
    transfer addr recvAddr amount

-- The mine rate of bitcoin is 10 min (600s).
-- This is just demonstration, so I set it to only 4s.
mine_rate = 4000

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
    let hashed = showBS . Crypto.hash $ concat [prev_id, showBS timestamp, m_root, origin, showBS nonce]
    if take bits hashed == replicate bits '0'
        then return $ BlockHeader prev_id timestamp m_root bits nonce
        else hash_calculate origin prev_id m_root bits (nonce + 1)


-- Adjust the difficulty of mining process                                
adjust_diff :: IO Int                                
adjust_diff = do
    blk  <- last_block
    let b = bits (blockHeader blk)
    let t = timestamp (blockHeader blk)
    time <- now
    if t + mine_rate > time then return $ b - 1 else return $ b + 1

-- Synchronize local ledger with the world state ledger
-- 1. First, request block_height number from connected peers
-- 2. Check height, select the highesh one (longest ledger), says, from Node X
-- 3. Send a get request to Node X with current local block_height
-- 4. Receive [Block] from Node X that contains array of missing blocks
-- 5. Verify recursively from latest block to current local block height
-- 6. If Verified, update, or re select ledger.
-- Consider the re-select process, and the synchronize time, if larger than mine_rate, the process need to reroll
sync_chain :: NodeState -> IO ()
sync_chain st = do
    peers <- tryReadMVar (_peers st)
    let socks = fromJust peers
    sendNetwork socks "blocks?"
    -- res <- recv


-- | Go live a node, with empty chain, peers and txn_pool
go_live p2p_port = do
    lambdaDb   <- start_lmdb
    sock       <- listenOn p2p_port
    txn_pool   <- newMVar mempty
    peers      <- newMVar mempty
    -- Initiate Node State
    let state  = NodeState {
        _db    = lambdaDb,
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

-- | Handle incoming requests    
req_handle :: Socket -> NodeState -> IO ()    
req_handle sock st = do
    raw <- recv sock 1024
    unless (null raw) $ do
        case (rawToMsg raw) of          
            TxnReq txn -> modifyMVarMasked_ (_pool st) $ \txns -> return $ expand_pool txn txns
            BlockReq b -> do
                -- TODO: verify block and clear pool
                print "received a block request, adding to db.."
                save_block b (_db st)
                print "Done"
            ChainInfo  -> do
                print "received a block query.."
                blocks <- block_height
                sendAll sock blocks
            Raw m      -> print m
