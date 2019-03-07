{-# LANGUAGE OverloadedStrings #-}

module Dash where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.List
import qualified Data.HashMap.Strict as M
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline
import System.Environment
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Network.Connection
import Network.Node
import Transaction
import Block


-- -----------------------------------------------------------------------------
-- Dash is a Command Line Interface that helps interact with Lambda-Chain
-- -----------------------------------------------------------------------------

-- | Define Type of Commands
data Cmd = Conn String String
         | Mine
         | Peers
         | Block_ Int
         | BlockHeight
         | Tx_Pool
         | Txn String Int
         | Raw String
         deriving (Show, Eq)

-- | Parse from input -> Command Type         
toCmd :: Maybe String -> Cmd
toCmd Nothing = Raw "Nothing"
toCmd (Just input)
    | isInfixOf "connect"  input = Conn (head args) (last args)
    | isInfixOf "transfer" input = Txn  (head args) (read $ last args :: Int)
    | isInfixOf "mine"     input = Mine
    | isInfixOf "peers"    input = Peers
    | isInfixOf "tx-pool"  input = Tx_Pool
    | isInfixOf "block"    input = Block_ (read $ head args :: Int)
    | isInfixOf "height"   input = BlockHeight
    | otherwise = Raw input
    where args = tail $ words input

-- | The REPL interface
-- dash :: PortNumber -> IO ()
dash p2p_port = do
    node <- go_live p2p_port
    runInputT defaultSettings $ loopCmd node

loopCmd :: NodeState -> InputT IO ()
loopCmd st = do
    minput <- getInputLine "> "
    case toCmd minput of
        Conn h p -> liftIO $ do
            sock <- connect_ (h, p)
            modifyMVarMasked_ (_peers st) $ \lst -> return $ Peer sock 0:lst
        Peers    -> liftIO $ tryReadMVar (_peers st) >>= print
        Tx_Pool  -> liftIO $ tryReadMVar (_pool st)  >>= print
        Txn a m  -> liftIO $ do
            socks   <- tryReadMVar (_peers st)
            maybeTx <- send_to (C.pack a) m
            case maybeTx of
                Nothing -> print "invalid Tx"
                Just tx -> do
                    modifyMVarMasked_ (_pool st) $ \txs -> return $ expand_pool tx txs
                    sendNetwork (fromJust socks) (C.append "txn:" (showBS tx))
        Mine     -> liftIO $ do
            txs <- tryReadMVar (_pool st)
            block <- mine_block $ fromJust txs
            print "A new block was mined, saving & sending to network.."
            Block.save block (_db st)
            socks <- tryReadMVar (_peers st)
            sendNetwork (fromJust socks) (C.append "block:" (showBS block))
        Block_ i -> liftIO $ do
            block <- find_by_index $ C.append "block#" (showBS i)
            print block
        BlockHeight -> liftIO $ (blocks $ _db st) >>= print
        Raw m    -> liftIO $ do
            socks <- tryReadMVar (_peers st)
            sendNetwork (fromJust socks) (C.pack m)    
    loopCmd st

-- -----------------------------------------------------------------------------
-- Http Server
-- -----------------------------------------------------------------------------

app :: Application
app _ respond = do
-- app _ respond = respond index
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hi, this is a sample message from Lambda Http Server!"

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
