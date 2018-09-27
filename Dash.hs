{-# LANGUAGE OverloadedStrings #-}

module Dash where

import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline
import Prelude hiding (takeWhile, dropWhile, tail)

import Network.Connection
import Network.Node
import Transaction
import Block


-- -----------------------------------------------------------------------------
-- Dash is a Command Line Interface that helps interact with Lambda-Chain

-- | The REPL interface
-- dash :: PortNumber -> IO ()
dash p2p_port = do
    node <- go_live p2p_port
    forkIO $ conn_handle node
    runInputT defaultSettings $ loopCmd node

loopCmd :: NodeState -> InputT IO ()
loopCmd st = do
    minput <- getInputLine "> "
    case minput of
        Nothing    -> return ()
        Just input -> liftIO $ do
            socks <- tryReadMVar (_peers st)
            sendNetwork (fromJust socks) (pack input)
    loopCmd st       

sycnChain :: Socket -> Blockchain -> IO ()    
sycnChain sock bc = do
    let msg = append "sync_chain:" (pack $ show bc)
    sendAll sock msg
    threadDelay 2048
    res <- recv sock 1024
    print res

-- -----------------------------------------------------------------------------
-- | Logics go here

-- | Handle Connections
conn_handle :: NodeState -> IO ()
conn_handle st = do
    threadDelay 1000000
    conn <- tryReadMVar (_peers st)
    forM_ (fromJust conn) $ \s -> req_handle s st

req_handle :: Socket -> NodeState -> IO ()    
req_handle sock st = do
    msg <- recv sock 1024
    case (takeWhile (/=':') msg) of
        "close"        -> do
            sendAll sock "Disconnected"
            close sock
        "blocks"       -> do
            chain <- tryReadMVar (_chain st)
            sendAll sock (pack $ show chain)            
        "transactions" -> do
            txns  <- tryReadMVar (_pool st)
            sendAll sock (pack $ show txns)
        "sync_chain"   -> do
            let recv_chain = read (unpack . tail $ dropWhile (/=':') msg) :: Blockchain
            chain <- tryReadMVar (_chain st)
            up_chain <- replace_chain recv_chain $ fromJust chain
            sendAll sock $ pack $ show up_chain
        "add_block"    -> do
            mined <- mineBlock genesis_block (tail $ dropWhile (/=':') msg) 0
            chain <- tryReadMVar (_chain st)
            let up_chain = Node mined (fromJust chain)
            conn  <- tryReadMVar (_peers st)
            forM_ (fromJust conn) $ \p -> sycnChain p up_chain           
        m -> sendAll sock "Acknowledged"
