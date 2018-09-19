{-# LANGUAGE OverloadedStrings #-}

module Address where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Crypto

data Address = Address {
    keyPair :: KeyPair,
    hexAddr  :: ByteString,
    balance :: Integer
} deriving (Eq, Show, Read)

new_addr :: IO Address
new_addr = do
    keys <- regKeys
    let hexAddr = hexPubKey_ $ fst keys
    return $ Address keys hexAddr 50