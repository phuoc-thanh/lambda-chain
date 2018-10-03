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
    from   :: ByteString,
    to     :: ByteString,
    amount :: Integer
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader {
    txnIndex :: Integer,
    txnTime  :: Integer,
    txnSign  :: Signature
} deriving (Eq, Show, Read)

type TransactionPool = [Transaction]

transfer :: Address -> ByteString -> Integer -> IO (Maybe Transaction)
transfer sender recvAddr amount
    | amount > (balance sender) = return Nothing
    | otherwise   = do
        timestamp <- now
        let data_ = hash . append (hexAddr sender) . append recvAddr $ showBS amount
        sign_     <- sign (snd $ keyPair sender) data_
        let head_ = TransactionHeader 0 timestamp sign_
        return    . Just $ Transaction head_ (hexAddr sender) recvAddr amount

expand_pool :: Transaction -> TransactionPool -> TransactionPool
expand_pool txn pool = if verify_txn txn then txn : pool else pool

reduce_pool :: [Transaction] -> TransactionPool -> TransactionPool
reduce_pool txns pool = pool \\ txns

-- | Verify a Signed Trasaction from given signature and data        
verify_txn :: Transaction -> Bool
verify_txn txn = verify (fromJust . getPubKey_ $ from txn) (txnSign $ header txn) (hash_txn txn)

-- | Hash transaction data with SHA256
hash_txn :: Transaction -> Digest SHA256
hash_txn txn = hash . append (from txn) $ append (to txn) (showBS $ amount txn)    

-- | Current time in millisecond
now :: IO Integer
now = round <$> (*1000) <$> getPOSIXTime

-- | Show ByteString
showBS :: Show a => a -> ByteString
showBS = pack . show