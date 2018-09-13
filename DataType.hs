
{-# LANGUAGE OverloadedStrings #-}

module DataType where

import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

import Crypto.Hash

-- Account {public_key, private_key}    
newtype Account = Account Integer

-- Default mine rate: 5min
mine_rate = 300

-- Transaction
data Transaction = Transaction {
    from :: Account,
    to :: Account,
    amount :: Integer
}

-- Block {timestamp, last_hash, hash, data, nonce}
data Block = Block {
    timestamp  :: ByteString,
    last_hash  :: ByteString,
    block_hash :: ByteString,
    block_data :: ByteString,
    nonce      :: ByteString,
    block_diff :: ByteString }
    deriving (Show, Eq, Read)

-- To be replace with a merkle tree    
data Blockchain = Genesis Block | Node Block Blockchain deriving (Show, Eq, Read)

genesis_block = Block "1536570561" "None" "f1rst_h4sh" "genesis-data" "0" "000"
init_chain = Genesis genesis_block

-- test data
next_block = Block "1536570561" "f1rst_h4sh" "next_h4sh" "next-data" "0" "000"
other_block = Block "1536570561" "next_h4sh" "other_h4sh" "next-data" "0" "000"
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

-- Hash last block and data
hash_ :: ByteString -> Block -> ByteString -> ByteString -> Digest SHA256
hash_ t b d n = hashWith SHA256 $ C.append t                  -- time in miliseconds 
                                $ C.append ":"                
                                $ C.append (block_hash b)     -- lash hash (block hash of last block)
                                $ C.append ":"
                                $ C.append d                  -- data
                                $ C.append ":"
                                $ C.append n              -- nonce
                                $ C.append ":" (block_diff b)

adjust_diff :: Block -> Integer -> ByteString                                
adjust_diff block time
    | (read (C.unpack $ timestamp block) :: Integer) + mine_rate > time = C.append "0" (block_diff block)
    | otherwise = C.init (block_diff block)

mineBlock :: Block -> ByteString -> Integer -> IO Block
mineBlock lastBlock input nonce = do
    now <- getPOSIXTime
    let timestamp = C.take 10 $ toBS now
    let hashed = toBS $ hash_ timestamp lastBlock input (toBS nonce)
    let diff = adjust_diff lastBlock (read $ C.unpack timestamp :: Integer)
    if C.take (C.length diff) hashed == diff
        then return $ Block (toBS now) (block_hash lastBlock) hashed input (toBS nonce) diff
        else do
            mineBlock lastBlock input (nonce + 1)

-- Other utils
toBS :: Show a => a -> ByteString
toBS = C.pack . show