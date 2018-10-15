{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Crypto
import Address

import Data.ByteString (ByteString)
import Data.ByteString.Char8 hiding (foldl')
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX

data Transaction = Transaction {
    header :: TransactionHeader,
    input  :: ByteString,
    output :: ByteString,
    amount :: Int
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader {
    txTime  :: Integer,
    txSign  :: Signature
} deriving (Eq, Show, Read)

type TransactionPool = [Transaction]

transfer :: Address -> ByteString -> Int -> IO (Maybe Transaction)
transfer sender recvAddr amount
    | amount > (balance sender) = do
        print "Invalid Transaction"
        return Nothing
    | otherwise   = do
        timestamp <- now
        let data_ = hash . append (hexAddr sender) . append recvAddr $ showBS amount
        sign_     <- sign (snd $ keyPair sender) data_
        let head_ = TransactionHeader timestamp sign_
        return    . Just $ Transaction head_ (hexAddr sender) recvAddr amount

expand_pool :: Transaction -> TransactionPool -> TransactionPool
expand_pool tx pool = if verify_tx tx then tx : pool else pool

reduce_pool :: [Transaction] -> TransactionPool -> TransactionPool
reduce_pool txs pool = pool \\ txs

-- | Verify a Signed Trasaction from given signature and data        
verify_tx :: Transaction -> Bool
verify_tx tx = verify (fromJust . getPubKey_ $ input tx) (txSign $ header tx) (hash_tx tx)

-- | Verify a batch of Transactions
verify_txs :: [Transaction] -> Bool
verify_txs txs = foldl' (&&) True (verify_tx <$> txs)

-- | Hash transaction data with SHA256
hash_tx :: Transaction -> Digest SHA256
hash_tx = hash . showBS

-- | Hash transaction data with SHA256, in Bytestring form
hash_tx' :: Transaction -> ByteString
hash_tx' = showBS . hash_tx

-- | Current time in millisecond
now :: IO Integer
now = round <$> (*1000) <$> getPOSIXTime

-- | Number of transactions
size :: [ByteString] -> Int
size = Data.List.length

-- | Show ByteString
showBS :: Show a => a -> ByteString
showBS = pack . show