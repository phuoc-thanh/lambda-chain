{-# LANGUAGE OverloadedStrings #-}

module Network.Message where

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Prelude hiding (takeWhile, dropWhile, tail)    
import Network.Connection
import Block
import Transaction

-- | Message Type: Raw, Request , Response
data Msg = Raw | BlockReq Block | BlockIdx Int | TxnReq Transaction

rawToMsg :: ByteString -> Msg
rawToMsg m = case (takeWhile (/=':') m) of
    "block" -> BlockReq (read data_ :: Block)
    "_id"   -> BlockIdx (read data_ :: Int)
    "txn"   -> TxnReq   (read data_ :: Transaction)
    raw     -> Raw
    where data_ = unpack . tail $ dropWhile (/=':') m