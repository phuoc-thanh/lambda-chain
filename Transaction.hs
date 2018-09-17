{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Address
import DataType

import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import Crypto.Random.Types
import Crypto.PubKey.ECC.Prim
import Crypto.Hash
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Char8
import qualified Data.Serialize as S
import Data.Monoid


data Transaction = Transaction {
    header :: TransactionHeader,
    from :: ByteString,
    to :: ByteString,
    amount :: Integer
    -- input :: ByteString,
    -- outputs :: [(ByteString, Integer)]
} deriving (Eq, Show, Read)

data TransactionHeader = TransactionHeader{
    txn_index :: Integer,
    timestamp :: Integer,
    signature :: ByteString
} deriving (Eq, Show, Read)

transfer :: Address -> ByteString -> Integer -> IO Transaction
transfer sender recvKey amount
    | amount > (balance sender) = do
        return $ Transaction (TransactionHeader 0 0 "Invalid") (pubKey sender) recvKey amount
    | otherwise = do
        timestamp <- now
        -- let signed = sign (toPrivateKey $ keyPair sender) SHA256 (S.encode "datax")
        let head = TransactionHeader 0 timestamp (showBS "data")
        return $ Transaction head (pubKey sender) recvKey amount

test = do
    sender <- regAddress
    receiver <- regAddress
    return $ transfer sender (pubKey receiver) 51