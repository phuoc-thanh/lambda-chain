{-# LANGUAGE OverloadedStrings #-}

module Network.Node where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe

import Address
import Block
import Transaction
import Persistence
import Network.Connection

data NodeInfo = NodeInfo {
    _host :: HostName,
    _port :: ServiceName,
    _addr :: Address
} deriving (Show, Read, Eq)

data NodeState = NodeState {
    _chain :: MVar Blockchain,
    _peers :: MVar [Socket],
    _pool  :: MVar TransactionPool
}

data NodeEnv = NodeEnv {
    node_info  :: NodeInfo,
    node_state :: NodeState
}

-- node_addr = ("127.0.0.1", "4747")
node_addr h p = append h $ append ":" p

initNode host port = do
    (txn, ref) <- open_lmdb "#"
    addr <- new_addr
    put txn ref (node_addr host port, pack $ show addr)
    commit_txn txn
    -- return $ NodeInfo "127.0.0.1" "4747" addr

getInfo = do
    db <- start_lmdb
    info <- Persistence.find db "#" "127.0.0.1:4747"
    return $ (read . unpack $ fromJust info :: Address)