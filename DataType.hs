
{-# LANGUAGE OverloadedStrings #-}

module DataType where

import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)

-- Account {public_key, private_key}    
newtype Account = Account Integer


-- Transaction
data Transaction = Transaction {
    from :: Account,
    to :: Account,
    amount :: Integer
}

-- Block {timestamp, last_hash, hash, data}
-- Integer should be replace by POSIXTime
data Block = Block Integer ByteString ByteString ByteString deriving Show

genesis = Block 1536570560 "None" "f1rst_h4sh" "base"