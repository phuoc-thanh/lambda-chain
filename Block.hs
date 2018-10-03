
{-# LANGUAGE OverloadedStrings #-}

module Block where

import Address    
import Persistence

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Crypto.PubKey.ECC.ECDSA
import Control.Concurrent

import Crypto.Hash
import Transaction

-- As walking around to find a standard for block/transaction definition,
-- this time I try to follow Cryptonote/Monero chain design.
-- https://cryptonote.org/standards/
-- But having some drops I've made for simplicity of lambda chain

-- The mine rate of bitcoin is 10 min (600s).
-- This is just demonstration, so I set it to only 3s.
mine_rate = 3

-- | Block Structure

-- A block consists of three parts:
-- - block header,
-- - base transaction body,
-- - list of transaction identifiers.	
-- The list starts with the number of transaction identifiers that it contains.
data Block = Block {
    header     :: BlockHeader,
    txns       :: [Transaction],
    -- block_hash :: ByteString,
    -- block_diff :: ByteString
} deriving (Show, Eq, Read)

-- | Calculation of Block Identifier

-- The identifier of a block is the result of hashing the following data with Keccak:

-- - size of [block_header, Merkle root hash, and the number of transactions] in bytes (varint)
-- - block_header,
-- - Merkle root hash,
-- - number of transactions (varint).

-- The goal of the Merkle root hash is to "attach" the transactions
-- referred to in the list to the block header: once the Merkle root
-- hash is fixed, the transactions cannot be modified.

data BlockHeader = BlockHeader {
    -- major_version :: Int,
    -- minor_version :: Int,
    prev_id   :: ByteString,
    timestamp   :: Integer,
    nonce       :: Integer
} deriving (Show, Eq, Read)

-- Chain of Blocks / List of Block_Hash
type Blockchain = [Block]

genesis_header = BlockHeader "genesis" "th1s" "n0n3" 1538583356613 0
genesis_block  = Block genesis_header [] "f1rst_h4sh" "000"
init_chain = [genesis_block]

is_valid_block :: Block -> Bool
is_valid_block = undefined

-- | Recursive validate a chain:
-- 
-- 1. Verify the hash value of current block by using hash_ function, if pass: Go to step (2), else return False.
-- 2. Verify the last_hash of current block with block_hash of older block, if pass: Recursive verify older block
-- 3. Return False if block doesn't match condition of step (1) and (2)
is_valid_chain :: Blockchain -> Bool
 -- Case of Genesis
is_valid_chain (b:[]) = (b == genesis_block)
is_valid_chain (b:p:chain)  -- Case of multiple Nodes
    | (showBS $ hash_ (showBS $ timestamp b) p (block_data b) (nonce b)) /= (block_hash b) = False
    | (last_hash b) == (block_hash p) = is_valid_chain (p:chain)
    | otherwise = False 


-- Verify and update to new longer chain if the new chain valids
replace_chain :: Blockchain -> Blockchain -> IO Blockchain
replace_chain new_c cur_c
    | (length new_c <= length cur_c) = return cur_c
    | (is_valid_chain new_c) /= True = return cur_c
    | otherwise = return new_c

-- Hash a block base on block's data and last_block's data
hash_ :: ByteString -> Block -> ByteString -> ByteString -> Digest SHA256
hash_ t b d n = hashWith SHA256 $ C.append t $ C.append ":"                 -- time in miliseconds
                                $ C.append (block_hash b) $ C.append ":"    -- lash_hash (block_hash of last block)
                                $ C.append d $ C.append ":"                 -- data
                                $ C.append n $ C.append ":" (block_diff b)  -- nonce n difficulty

-- Adjust the difficulty of mining process                                
adjust_diff :: Block -> Integer -> ByteString                                
adjust_diff block time
    | (timestamp block) + mine_rate > time = C.append "0" (block_diff block)
    | otherwise = C.init (block_diff block)

mineBlock :: Block -> ByteString -> Integer -> IO Block
mineBlock lastBlock input nonce = do
    timestamp  <- now
    let hashed = showBS $ hash_ (showBS timestamp) lastBlock input (showBS nonce)
    let diff   = adjust_diff lastBlock timestamp
    if C.take (C.length diff) hashed == diff
        then return $ Block timestamp (block_hash lastBlock) hashed input (showBS nonce) diff
        else mineBlock lastBlock input (nonce + 1)

-- saveBlock = do
--     db <- openDb
--     -- push_single db ((block_hash genesis_block), showBS genesis_block)
--     -- threadDelay 3000000
--     updated <- try 2 $ find db "@" $ "f1rst_h4sh"
--     return updated