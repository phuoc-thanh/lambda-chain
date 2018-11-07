
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Block where

import Address    
import Persistence

import Data.ByteString (ByteString)
import Data.ByteString.Char8 hiding (find)
import Control.Concurrent
import Prelude hiding (append, length, init, replicate, take)

import Crypto
import Transaction

-- The Design of Lambda-chain should (in future) follow the:
-- The Cryptonote's standard: https://cryptonote.org/standards/
-- It's now a mix of bitcoin and monero, having some drops for simplicity.

-- -----------------------------------------------------------------------------
-- | Block Structure
-- -----------------------------------------------------------------------------

-- A block consists of four parts:
-- - block header,
-- - base transaction body,
-- - the number of transactions,
-- - list of transaction identifiers.
data Block = Block {
    blockHeader :: BlockHeader,  -- Metadata
    -- baseTx      :: Transaction,  -- Coin base Transaction, or Miner Transaction
    origin      :: ByteString,   -- to be replaced with the baseTx as above
    txNum       :: Int,          -- Number of Transactions
    txHashes    :: [ByteString]  -- List of Transaction Identifiers
} deriving (Show, Eq, Read)

data BlockHeader = BlockHeader {
    prevId      :: ByteString, -- Identifier of the previous block
    timestamp   :: Int,        -- The creation time of block
    merkleRoot  :: ByteString, -- Merkle Root of transactions
    bits        :: Int,        -- Difficulty of block
    nonce       :: Int         -- Nonce number
} deriving (Show, Eq, Read)

-- | The identifier of a block

-- Is the result of hashing the following data with SHA256 hash function:
-- - size of [block_header, Merkle root hash, and the number of transactions] in bytes,
-- - block_header,
-- - Merkle root hash,
-- - number of transactions.
hash_id :: BlockHeader -> [ByteString] -> ByteString
hash_id header txs = showBS . hash $ append (showBS $ length blob) blob
    where blob = append (showBS header) $ append (merkle_root txs) (showBS $ size txs)


-- Chain of Blocks / List of Block_Hash
type Blockchain = [ByteString]

genesis_header = BlockHeader "genesis" 1538583356613 "no-merkle-root" 3 0
genesis_block  = Block genesis_header "f1rstM1n3r" 0 []
init_chain = [genesis_block]

-- Is a block valid?
is_valid_block :: ByteString -> Block -> Block -> Bool
is_valid_block block_id block prev
    | block_id /= Block.hash_id (blockHeader block) (txHashes block) = False
    | prev_id  /= Block.hash_id (blockHeader prev)  (txHashes prev)  = False
    -- | -- validate transactions here
    | otherwise = True
    where prev_id = prevId $ blockHeader block

-- | Recursive validate a chain:
is_valid_chain :: Blockchain -> IO Bool
is_valid_chain (b:[]) = do
    block <- find_by_id b
    let b0 = read $ unpack block :: Block
    if b0 == genesis_block then return True else return False
is_valid_chain (blk:prev:chain) = do
    block <- find_by_id blk
    prevB <- find_by_id prev
    let b1 = read $ unpack block :: Block
    let b2 = read $ unpack prevB :: Block
    case is_valid_block blk b1 b2 of
        False -> return False
        True  -> is_valid_chain (prev:chain)


-- -- Verify and update block to latest chain
-- replace_chain :: Blockchain -> Blockchain -> IO Blockchain
-- accept_block blk = do
--     last_hash <- last_block_id
    

init_genesis = do
    db <- start_lmdb
    push_single db (Block.hash_id genesis_header [], showBS genesis_block)

-- -----------------------------------------------------------------------------
-- | Persistence
-- -----------------------------------------------------------------------------

-- | On "#" dabatase, lmdb writes pair of (block_height, block_id)
-- block_height is also considered as index of block
-- that presents how old a block.

-- | On "@" database, lmdb writes pair of (block_id, block)
-- This is the data inside of a block

find_by_id block_id = find' "@" block_id

block_height = find' "#" "block_seq"

last_block_id = do
    h <- block_height
    find' "#" (append "block_" h)

last_block :: IO Block
last_block = do
    block_id <- last_block_id
    val      <- find' "@" block_id
    let block = readBS $ val :: Block
    return block

prev_block blk = find' "@" (prevId $ blockHeader blk)


-- Save a block to db
save_block blk db = do
    let block_id = Block.hash_id header txs
    let block_v  = append "block#" (showBS blk)
    push_single db (block_id, block_v)
    where header = blockHeader blk
          txs    = txHashes    blk