{-# LANGUAGE OverloadedStrings #-}

module Network.Message where

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Prelude hiding (takeWhile, dropWhile, tail)    
import Network.Connection
import Block
import Transaction

-- | Message Type: Raw, Request , Ask/Query
-- Raw:  Nothing special, just text.
-- Req:  Updating request for a block, a transaction or ledger
-- Info: Query block info, transaction info... expecting to receive a response
data Msg = Raw ByteString | BlockReq Block | TxnReq Transaction | ChainInfo | BlockInfo Int

rawToMsg :: ByteString -> Msg
rawToMsg m = case (takeWhile (/=':') m) of
    "chain?"  -> ChainInfo
    "block?"  -> BlockInfo (read data_ :: Int)
    "block"   -> BlockReq  (read data_ :: Block)
    "txn"     -> TxnReq    (read data_ :: Transaction)
    msg       -> Raw msg
    where data_ = unpack . tail $ dropWhile (/=':') m