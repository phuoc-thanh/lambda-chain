
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

{- The Design of Lambda-chain should (in future) follow the:
   The Cryptonote's standard: https://cryptonote.org/standards/
   It's now a mix of bitcoin and monero, having some drops for simplicity.-}

-- -----------------------------------------------------------------------------
-- | Block Structure
-- -----------------------------------------------------------------------------

{- A block consists of four parts:
    - block header,
    - base transaction body,
    - the number of transactions,
    - list of transaction identifiers. -}
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

{- Is the result of hashing the following data with SHA256 hash function:
    - size of [block_header, Merkle root hash, and the number of transactions] in bytes,
    - block_header,
    - Merkle root hash,
    - number of transactions.-}
hash_id :: BlockHeader -> [ByteString] -> ByteString
hash_id header txs = showBS . hash $ append (showBS $ length blob) blob
    where blob = append (showBS header) $ append (merkle_root txs) (showBS $ size txs)


-- | Chain of Blocks / List of Block_Hash
type Blockchain = [ByteString]

genesis_header = BlockHeader "genesis" 1538583356613 "no-merkle-root" 4 0
genesis_block  = Block genesis_header "f1rstM1n3r" 0 []

-- | Validate a Block
is_valid_block :: Block -> Block -> Bool
is_valid_block Block{..} (Block h o n txs)
    | (prevId blockHeader) /= Block.hash_id h txs = False
    | (take (bits blockHeader) hashed) /= replicate (bits blockHeader) '0' = False
    -- | -- validate transactions here
    | otherwise = True
    where hashed = showBS . hash . append (prevId blockHeader) 
                 . append (showBS $ timestamp blockHeader) 
                 . append (merkleRoot blockHeader)
                 $ append origin (showBS $ nonce blockHeader)

-- | Recursive validate a chain of blocks
is_valid_chain :: [Block] -> Bool
is_valid_chain (b:[]) = True -- to be revise
is_valid_chain (b:b1:bs)
    | (is_valid_block b b1) /= True = False
    | otherwise = is_valid_chain (b1:bs)


-- | Verify and update block to latest chain
-- replace_chain :: Blockchain -> Blockchain -> IO Blockchain
-- accept_block blk = do
--     last_hash <- last_block_id
    

init_genesis = do
    reset_lmdb
    db <- start_lmdb
    push_single db (append "block#" $ Block.hash_id genesis_header [], showBS genesis_block)

-- -----------------------------------------------------------------------------
-- | Persistence
-- -----------------------------------------------------------------------------

{- On "#" dabatase, lmdb writes pair of (block_height, block_id)
    block_height is also considered as index of block
    that presents how old a block.

 - On "@" database, lmdb writes pair of (block_id, block)
    This is the data inside of a block -}

find_by_id block_id = find' "@" block_id

find_by_index block_height = do
    block_id <- find' "#" block_height
    find' "@" block_id

-- | Quick query block height
block_height = find' "#" "block"

-- | Quick query last block id
last_block_id = do
    h <- block_height
    find' "#" (append "block#" h)

-- | Quick query last block
last_block :: IO Block
last_block = do
    block_id <- last_block_id
    val      <- find' "@" block_id
    let block = readBS $ val :: Block
    return block

prev_block blk = find' "@" (prevId $ blockHeader blk)

-- | Save a block to db
save_block blk db = do
    let block_k = append "block#" $ Block.hash_id header txs
    let block_v  = showBS blk
    push_single db (block_k, block_v)
    where header = blockHeader blk
          txs    = txHashes    blk