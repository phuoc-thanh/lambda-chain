
{-# LANGUAGE OverloadedStrings #-}

module DataType where

import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

import Crypto.Hash

-- Account {public_key, private_key}    
newtype Account = Account Integer

-- Transaction
data Transaction = Transaction {
    from :: Account,
    to :: Account,
    amount :: Integer
}

-- Block {timestamp, last_hash, hash, data}
-- Integer should be replace by POSIXTime
data Block = Block {
    timestamp :: ByteString,
    last_hash :: ByteString,
    block_hash :: ByteString,
    block_data :: ByteString }
    deriving (Show, Eq, Read)

-- To be replace with a merkle tree    
data Blockchain = Genesis Block | Node Block Blockchain deriving (Show, Eq, Read)

genesis_block = Block "1536570560" "None" "f1rst_h4sh" "genesis-data"
init_chain = Genesis genesis_block

-- test data
next_block = Block "1536570561" "f1rst_h4sh" "next_h4sh" "next-data"
other_block = Block "1536570561" "next_h4sh" "other_h4sh" "next-data"
chain2 = Node next_block (Genesis genesis_block)
chain3 = Node other_block (Node next_block (Genesis genesis_block))

-- to be remove
add_block block chain = Node block chain

-- recursive validate a chain
is_valid_chain :: Blockchain -> Bool
is_valid_chain (Genesis b)
    | b == genesis_block = True
    | otherwise = False
is_valid_chain (Node b (Genesis g))
    | (last_hash b) == (block_hash g) = is_valid_chain (Genesis g)
    | otherwise = False 
is_valid_chain (Node b (Node n chain))
    | (last_hash b) == (block_hash n) = is_valid_chain (Node n chain)
    | otherwise = False 

chain_length (Genesis b) = 1
chain_length (Node b (Genesis g)) = 2
chain_length (Node b (Node n chain)) = 1 + chain_length (Node n chain)

replace_chain :: Blockchain -> Blockchain -> IO Blockchain
replace_chain new_c cur_c
    | (chain_length new_c <= chain_length cur_c) = return cur_c
    | (is_valid_chain new_c) /= True = return cur_c
    | otherwise = return new_c

-- Build a to-be-hashed string
hash_string t b d = C.append (C.pack $ show t) . C.append ":" $ C.append (block_hash b) $ C.append ":" d

mineBlock :: Block -> C.ByteString -> IO Block
mineBlock lastBlock input = do
    now <- getPOSIXTime
    let to_hash = hash_string now lastBlock input
    return $ Block (C.pack $ show now) (block_hash lastBlock) (C.pack $ show $ hashWith SHA256 to_hash) input
