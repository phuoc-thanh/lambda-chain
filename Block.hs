
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Address    
import Persistence

import Data.ByteString (ByteString)
import Data.ByteString.Char8 hiding (find)
import Control.Concurrent
import Prelude hiding (append, length)

import Crypto
import Transaction

-- The Design of Lambda-chain should (in future) follow the:
-- The Cryptonote's standard: https://cryptonote.org/standards/
-- It's now a mix of bitcoin and monero, having some drops for simplicity.

-- The mine rate of bitcoin is 10 min (600s).
-- This is just demonstration, so I set it to only 3s.
mine_rate = 3

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
    prevId     :: ByteString, -- Identifier of the previous block
    timestamp  :: Int,        -- The creation time of block
    bits       :: Int,        -- Difficulty of block
    nonce      :: Int         -- Nonce number
} deriving (Show, Eq, Read)

-- | The identifier of a block

-- Is the result of hashing the following data with SHA256 hash function:
-- - size of [block_header, Merkle root hash, and the number of transactions] in bytes,
-- - block_header,
-- - Merkle root hash,
-- - number of transactions.
hash_block_id :: BlockHeader -> [ByteString] -> ByteString
hash_block_id header txs = showBS . hash $ append (showBS $ length blob) blob
    where blob = append (showBS header) $ append (merkle_root txs) (showBS $ size txs)


-- Chain of Blocks / List of Block_Hash
type Blockchain = [Block]

genesis_header = BlockHeader "genesis" 1538583356613 00 0
genesis_block  = Block genesis_header "f1rstM1n3r" 0 []
init_chain = [genesis_block]

-- Is a block valid?
is_valid_block :: ByteString -> Block -> Block -> Bool
is_valid_block block_id block prev
    | block_id /= hash_block_id (blockHeader block) (txHashes block) = False
    | prev_id  /= hash_block_id (blockHeader prev)  (txHashes prev)  = False
    -- | -- validate transactions here
    | otherwise = True
    where prev_id = prevId $ blockHeader block

-- | Recursive validate a chain:
-- 
-- 1. Verify the hash value of current block by using hash_ function, if pass: Go to step (2), else return False.
-- 2. Verify the last_hash of current block with block_hash of older block, if pass: Recursive verify older block
-- 3. Return False if block doesn't match condition of step (1) and (2)
-- is_valid_chain :: Blockchain -> Bool
is_valid_chain = undefined


-- -- Verify and update to new longer chain if the new chain valids
-- replace_chain :: Blockchain -> Blockchain -> IO Blockchain
replace_chain = undefined

-- -- Hash a block base on block's data and last_block's data


-- -- Adjust the difficulty of mining process                                
-- adjust_diff :: Block -> Integer -> ByteString                                
-- adjust_diff block time
--     | (timestamp block) + mine_rate > time = append "0" (block_diff block)
--     | otherwise = init (block_diff block)

-- mineBlock :: Block -> ByteString -> Integer -> IO Block
-- mineBlock lastBlock input nonce = do
--     timestamp  <- now
--     let hashed = showBS $ hash_ (showBS timestamp) lastBlock input (showBS nonce)
--     let diff   = adjust_diff lastBlock timestamp
--     if take (length diff) hashed == diff
--         then return $ Block timestamp (block_hash lastBlock) hashed input (showBS nonce) diff
--         else mineBlock lastBlock input (nonce + 1)


-- genesis hash: fc89748c6b236a6eb8adbf4407ee75eae8f9ef770b6222c56130145775f1dc33
init_genesis = do
    db <- start_lmdb
    push_single db (hash_block_id genesis_header [], showBS genesis_block)

find_block block_id = do
    (txn, dbi) <- open_lmdb "@"
    val <- find_ txn dbi block_id
    case val of
        Nothing -> print "Nothing - return from lmdb"
        value   -> print value
