{-# LANGUAGE OverloadedStrings #-}

module Network.Message where

data Msg = ReqBlocks
    | AddBlock
    | ReqTxn
    | ExpTxn
    | SyncBlocks     