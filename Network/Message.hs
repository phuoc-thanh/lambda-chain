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
-- Req:  Request for a block, a transaction or the ledger.
-- Info: Promote a block info, transaction info... expecting to receive a response.
data Msg = Raw ByteString | BlockReq Int | BlockInfo Block | TxnReq Transaction | ChainReq | ChainInfo Int

rawToMsg :: ByteString -> Msg
rawToMsg m = case (takeWhile (/=':') m) of
    "chain?"  -> ChainReq
    "block?"  -> BlockReq  (read data_ :: Int)
    "chain"   -> ChainInfo (read data_ :: Int)
    "block"   -> BlockInfo (read data_ :: Block)
    "txn"     -> TxnReq    (read data_ :: Transaction)
    msg       -> Raw msg
    where data_ = unpack . tail $ dropWhile (/=':') m