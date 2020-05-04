
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Block where

import Address    
import Persistence

import Data.ByteString (ByteString)
import Data.ByteString.Char8 hiding (find)
import Control.Concurrent
import Control.Monad (forM_, mapM_)
import Prelude hiding (append, length, init, replicate, take, concat)

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
hash_id header txs = showBS . Crypto.hash $ append (showBS $ length blob) blob
    where blob = concat [showBS header, merkle_root txs, showBS $ size txs]

hash :: Block -> Digest SHA256
hash Block{..} = Crypto.hash $ concat [prevId blockHeader,
                                showBS $ timestamp blockHeader,
                                merkleRoot blockHeader,
                                origin,
                                showBS $ nonce blockHeader]


-- | Chain of Blocks / List of Block_Hash
type Blockchain = [ByteString]

-- | Validate a Block: this_block -> previous_block -> True/False
is_valid :: Block -> Block -> Bool
is_valid blk@Block{..} (Block h o n txs)
    | (prevId blockHeader) /= Block.hash_id h txs = False
    | (take (bits blockHeader) hashed) /= replicate (bits blockHeader) '0' = False
    -- validate transactions here
    | otherwise = True
    where hashed = showBS $ Block.hash blk

-- | Recursive validate a chain of blocks
is_valid_chain :: [Block] -> Bool
is_valid_chain (b:[]) = True -- the last block, to be revise
is_valid_chain (b:b1:bs)
    | (is_valid b b1) /= True = False
    | otherwise = is_valid_chain (b1:bs)


-- | Verify and update block to latest chain
-- replace_chain :: Blockchain -> Blockchain -> IO Blockchain


-- -----------------------------------------------------------------------------
-- | Persistence
-- -----------------------------------------------------------------------------

{- On "#" dabatase, lmdb stores pair of (block_height, block_id)
    ---------------------------------
    |       key      |     value    |
    ---------------------------------
    |    block#0     |  block#c892  |
    |    block#1     |  block#d189  |
    |       ...      |     ...      |


 - On "@" database, lmdb stores pair of (block_id, block_data)
    ---------------------------------
    |       key      |     value    |
    ---------------------------------
    |   block#c892   |  block_data  |
    |   block#d189   |  block_data  |
    |       ...      |      ...     |

    -}

find_by_id block_id = find' "@" block_id

-- | Find a block by index / height
find_by_idx idx db = do
    block_id <- find db "#" idx
    find' "@" block_id

-- | Quick query last block id
last_block_id = do
    h <- find' "#" "blocks"
    find' "#" (append "block#" h)

-- | Quick query last block
last_block :: IO Block
last_block = do
    block_id <- last_block_id
    val      <- find' "@" block_id
    let block = readBS $ val :: Block
    return block

-- | Return last block with ldb env
last db = do
    height   <- find db "#" "blocks"
    block_id <- find db "#" $ append "block#" height
    val <- find db "@" block_id
    let last_block = readBS $ val :: Block
    return last_block

-- | Return previous block
prev blk = find' "@" (prevId $ blockHeader blk)

-- | Save a block to db
save :: Block -> Lambdadb -> IO ()
save blk@Block{..} db = do
    let block_k  = append "block#" $ Block.hash_id blockHeader txHashes
    let block_v  = showBS blk
    push_single db (block_k, block_v)

-- | Save multiple blocks at once
-- I don't use push_commit due to it's type, hashmap is unordered
-- The ledger must be consistency, each block always have it's height
saveM :: [Block] -> Lambdadb -> IO ()
saveM bs db = mapM_ (\b -> save b db) bs
