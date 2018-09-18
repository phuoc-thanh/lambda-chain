{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Cryptography
import DataType
import Address

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe

data Transaction = Transaction {
    header :: TransactionHeader,
    from :: PublicKey,
    to :: PublicKey,
    amount :: Integer
    -- input :: ByteString,
    -- outputs :: [(ByteString, Integer)]
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader{
    txn_index :: Integer,
    timestamp :: Integer,
    signature :: Signature
} deriving (Eq, Show, Read)

transfer :: Address -> PublicKey -> Integer -> IO (Maybe Transaction)
transfer sender recvKey amount
    | amount > (balance sender) = return Nothing
    | otherwise = do
        timestamp <- now
        let data_ = hash . append (pubKey sender) . append recvKey $ showBS amount
        sign_    <- sign (snd $ keyPair sender) (showBS data_)
        let head_ = TransactionHeader 0 timestamp sign_
        return . Just $ Transaction head_ (pubKey sender) recvKey amount

verify_txn :: Transaction -> Bool
verify_txn txn = verify (from txn) (signature $ header txn) data_
    where data_ = hash . append (from txn) . append (to txn) $ showBS amount

test = do
    sender <- new_addr
    receiver <- new_addr
    transfer sender (pubKey receiver) 49