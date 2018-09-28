{-# LANGUAGE OverloadedStrings #-}

module Dash where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline
import System.Environment

import Network.Connection
import Network.Node
import Transaction
import Block


-- -----------------------------------------------------------------------------
-- Dash is a Command Line Interface that helps interact with Lambda-Chain

-- | Define Type of Commands
-- data Arg = Host | Port

-- | The REPL interface
-- dash :: PortNumber -> IO ()
dash p2p_port = do
    node <- go_live p2p_port
    runInputT defaultSettings $ loopCmd node

loopCmd :: NodeState -> InputT IO ()
loopCmd st = do
    minput <- getInputLine "> "
    case minput of
        Nothing    -> return ()
        Just input -> liftIO $ do
            socks <- tryReadMVar (_peers st)
            sendNetwork (fromJust socks) (C.pack input)
    loopCmd st

-- -----------------------------------------------------------------------------
-- | Logics go here

