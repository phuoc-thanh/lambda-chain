
{-# LANGUAGE OverloadedStrings #-}

module DataType where

import Address    
import Persistence

import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Crypto.PubKey.ECC.ECDSA
import Control.Concurrent

import Crypto.Hash

-- The mine rate of bitcoin is 10 min (600s).
-- This is just demonstration, so I set it to only 3s.
mine_rate = 3

-- Block {timestamp, last_hash, hash, data, nonce}
data Block = Block {
    timestamp  :: Integer,
    last_hash  :: ByteString,
    block_hash :: ByteString,
    block_data :: ByteString,
    nonce      :: ByteString,
    block_diff :: ByteString }
    deriving (Show, Eq, Read)

-- To be replace with a merkle tree    
data Blockchain = Genesis Block | Node Block Blockchain deriving (Show, Eq, Read)

genesis_block = Block 1536570561000 "None" "f1rst_h4sh" "genesis-data" "0" "000"
init_chain = Genesis genesis_block

-- test data
next_block = Block 1536831680000 "f1rst_h4sh" "00610a6673b0baed2b47a203b33b24e1135637418ce450b6315d92a04d7e2783" "next-data" "539" "00"
other_block = Block 1536831936000 "f1rst_h4sh" "0ac1720a077bb02b872a4a42f5652cceca419829b74e7d451b55c17c0b3a4f69" "next-data" "15" "0"
chain2 = Node next_block (Genesis genesis_block)
chain3 = Node other_block (Node next_block (Genesis genesis_block))

-- to be remove
add_block :: Integer -> Blockchain -> IO Blockchain
add_block 0 (Node block chain) = return (Node block chain)
add_block n (Node block chain) = do
    new_block <- mineBlock block "test_data" 0 
    let new_chain = Node new_block (Node block chain)
    add_block (n - 1) new_chain

-- | Recursive validate a chain:
-- 
-- 1. Verify the hash value of current block by using hash_ function, if pass: Go to step (2), else return False.
-- 2. Verify the last_hash of current block with block_hash of older block, if pass: Recursive verify older block
-- 3. Return False if block doesn't match condition of step (1) and (2)
is_valid_chain :: Blockchain -> Bool
is_valid_chain (Genesis b)              -- Case of Genesis
    | b == genesis_block = True
    | otherwise = False
is_valid_chain (Node b (Genesis g))     -- Case of 1 Node + Genesis
    | (showBS $ hash_ (showBS $ timestamp b) g (block_data b) (nonce b)) /= (block_hash b) = False
    | (last_hash b) == (block_hash g) = is_valid_chain (Genesis g)
    | otherwise = False 
is_valid_chain (Node b (Node n chain))  -- Case of multiple Nodes
    | (showBS $ hash_ (showBS $ timestamp b) n (block_data b) (nonce b)) /= (block_hash b) = False
    | (last_hash b) == (block_hash n) = is_valid_chain (Node n chain)
    | otherwise = False 

chain_length (Genesis b) = 1
chain_length (Node b (Genesis g)) = 2
chain_length (Node b (Node n chain)) = 1 + chain_length (Node n chain)

-- Verify and update to new longer chain if the new chain valids
replace_chain :: Blockchain -> Blockchain -> IO Blockchain
replace_chain new_c cur_c
    | (chain_length new_c <= chain_length cur_c) = return cur_c
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
    timestamp <- now
    let hashed    = showBS $ hash_ (showBS timestamp) lastBlock input (showBS nonce)
    let diff      = adjust_diff lastBlock timestamp
    if C.take (C.length diff) hashed == diff
        then return $ Block timestamp (block_hash lastBlock) hashed input (showBS nonce) diff
        else mineBlock lastBlock input (nonce + 1)

-- | Show ByteString
showBS :: Show a => a -> ByteString
showBS = C.pack . show

-- | Current time in millisecond
now :: IO Integer
now = round <$> (*1000) <$> getPOSIXTime

saveBlock = do
    db <- openDb
    -- push_single db ((block_hash genesis_block), showBS genesis_block)
    -- threadDelay 3000000
    updated <- try 2 $ find db "@" $ "f1rst_h4sh"
    return updated