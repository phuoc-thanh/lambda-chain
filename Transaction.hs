{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Crypto
import Block
import Address

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.List
import Data.Maybe

data Transaction = Transaction {
    header :: TransactionHeader,
    from :: ByteString,
    to :: ByteString,
    amount :: Integer
    -- input :: ByteString,
    -- outputs :: [(ByteString, Integer)]
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader {
    txn_index :: Integer,
    timestamp :: Integer,
    signature :: Signature
} deriving (Eq, Show, Read)

type TransactionPool = [Transaction]

transfer :: Address -> ByteString -> Integer -> IO (Maybe Transaction)
transfer sender recvAddr amount
    | amount > (balance sender) = return Nothing
    | otherwise = do
        timestamp <- now
        let data_ = hash . append (hexAddr sender) . append recvAddr $ showBS amount
        sign_    <- sign (snd $ keyPair sender) data_
        let head_ = TransactionHeader 0 timestamp sign_
        return . Just $ Transaction head_ (hexAddr sender) recvAddr amount

expand_pool :: Transaction -> TransactionPool -> TransactionPool
expand_pool txn pool = txn : pool

reduce_pool :: [Transaction] -> TransactionPool -> TransactionPool
reduce_pool txns pool = pool \\ txns

-- | Verify a Signed Trasaction from given signature and data        
verify_txn :: Transaction -> Bool
verify_txn txn = verify (fromJust . getPubKey_ $ from txn) (signature $ header txn) (hash_txn txn)

-- | Hash transaction data with SHA256
hash_txn :: Transaction -> Digest SHA256
hash_txn txn = hash . append (from txn) $ append (to txn) (showBS $ amount txn)    

test = do
    sender <- new_addr
    receiver <- new_addr
    txn <- transfer sender (hexAddr receiver) 49
    return $ verify_txn $ fromJust txn