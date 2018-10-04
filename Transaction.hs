{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Crypto
import Address

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX

data Transaction = Transaction {
    header :: TransactionHeader,
    input  :: ByteString,
    output :: ByteString,
    amount :: Integer
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader {
    txTime  :: Integer,
    txSign  :: Signature
} deriving (Eq, Show, Read)

type TransactionPool = [Transaction]

transfer :: Address -> ByteString -> Integer -> IO (Maybe Transaction)
transfer sender recvAddr amount
    | amount > (balance sender) = return Nothing
    | otherwise   = do
        timestamp <- now
        let data_ = hash . append (hexAddr sender) . append recvAddr $ showBS amount
        sign_     <- sign (snd $ keyPair sender) data_
        let head_ = TransactionHeader timestamp sign_
        return    . Just $ Transaction head_ (hexAddr sender) recvAddr amount

expand_pool :: Transaction -> TransactionPool -> TransactionPool
expand_pool tx pool = if verify_txn tx then tx : pool else pool

reduce_pool :: [Transaction] -> TransactionPool -> TransactionPool
reduce_pool txns pool = pool \\ txns

-- | Verify a Signed Trasaction from given signature and data        
verify_txn :: Transaction -> Bool
verify_txn tx = verify (fromJust . getPubKey_ $ input tx) (txSign $ header tx) (hash_txn tx)

-- | Hash transaction data with SHA256
hash_txn :: Transaction -> Digest SHA256
hash_txn tx = hash $ showBS tx

-- | Calculate Merkle Root of transactions
merkle_root :: [Transaction] -> MerkleRoot ByteString
merkle_root txs = mtRoot . mkMerkleTree $ showBS <$> txs

-- | Current time in millisecond
now :: IO Integer
now = round <$> (*1000) <$> getPOSIXTime

-- | Show ByteString
showBS :: Show a => a -> ByteString
showBS = pack . show