{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv, Raw)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 hiding (find)
import Data.Maybe
import Prelude hiding (takeWhile, dropWhile, take, tail, null, length, replicate, concat)

import Address
import Block
import Crypto
import Transaction
import Persistence
import Network.Connection
import Network.Message


data NodeState = NodeState {
    _db     :: Lambdadb,
    _pool   :: MVar [Transaction],
    _peers  :: MVar [Node]
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
    put txn ref ("blocks"   , "1" )
    put txn ref ("block#1"  , append "block#" hashid)
    commit_txn txn
    (txn2, db) <- open_lmdb "@"
    put txn2 db (append "block#" hashid, showBS genesis_block)
    commit_txn txn2
    print $ append "Node is registered, node_addr: " (hexAddr addr)
    where
        hashid         = Block.hash_id genesis_header []
        genesis_block  = Block genesis_header "f1rstM1n3r" 0 []
        genesis_header = BlockHeader "genesis" 1538583356613 "no-merkle-root" 4 0

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

-- | Synchronize local ledger with the world state ledger

{- Detail steps of synchronize process:
1. First, request the "next_block" from connected peers
2. If nothing returned from result, then the node already has longest version of ledger
3. Else update the ledger with new block received, it may send up-to 4 blocks/response
4. Continue that process until get nothing in result -}

sync_chain :: NodeState -> IO ()
sync_chain st = do
    peers <- tryReadMVar (_peers st)
    -- Request a next block base on current ledger (block height)
    height <- blocks (_db st)
    sendNetwork (fromJust peers) $ append "block?:" height
    -- let pc = peer_select $ fromJust peers
    print "txt"


update :: [Block] -> NodeState -> IO ()
update bs st = do
    case is_valid_chain bs of
        False -> print "invalid chain"
        True  -> Block.saveM bs (_db st)
        

-- | Go live a node, with empty chain, peers and txn_pool
go_live p2p_port = do
    lambdaDb   <- start_lmdb
    txn_pool   <- newMVar mempty
    peers      <- newMVar mempty
    -- Initiate Node State
    let state   = NodeState {
        _db     = lambdaDb,
        _pool   = txn_pool,
        _peers  = peers
    }
    forkIO $ listenOn p2p_port (_peers state)
    forkIO $ conn_handle state
    return state

-- | Handle Connections
conn_handle :: NodeState -> IO ()
conn_handle st = do
    threadDelay 2000000
    peers <- tryReadMVar (_peers st)
    forM_ (fromJust peers) $ \peer -> req_handle peer st
    conn_handle st

-- | Handle incoming requests    
req_handle :: Node -> NodeState -> IO ()    
req_handle peer st = do
    let sock = _sock peer
    raw <- recv sock 1024
    unless (null raw) $ do
        case (rawToMsg raw) of     
            TxnReq txn -> modifyMVarMasked_ (_pool st) $ \txns -> return $ expand_pool txn txns
            BlockInfo b -> do
                print "received a new block, verifying.."
                p <- Block.last (_db st)
                case Block.is_valid b p of
                    False -> print "invalid block!"
                    True  -> Block.save b (_db st)
            BlockReq l -> do
                print "received a ledger's state request, selecting from db.."
                height <- blocks (_db st)
                sendAll sock $ append "chain:" height
            Raw m      -> print m

-- -----------------------------------------------------------------------------
-- | Persistence
-- -----------------------------------------------------------------------------

{- On "#" database, lmdb stores information of the running node
    ---------------------------------------
    |     key     |         value         |
    ---------------------------------------
    |  hex_addr   |  hex_address_of_node  |
    |  node_addr  |  address_of_node      |
    |  node_keys  |  key_pairs_of_node    |
    |  balance    |  balance_of_node      |
    |  blocks     |  block_height         |

--}

-- | Return the block height of current chain
blocks db = find db "#" "blocks"

-- | Get a range of blocks, from x to y (by height)
get_blocks x y db = mapM (\n -> Block.find_by_idx db) [x..y]

-- | Return the hex-version PublicKey of Node    
getPublicAddress = find' "#" "hex_addr"    