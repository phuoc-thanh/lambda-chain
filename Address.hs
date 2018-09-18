{-# LANGUAGE OverloadedStrings #-}

module Address where

import Data.ByteString (ByteString)
import Cryptography

data Address = Address {
    keyPair :: KeyPair,
    pubKey  :: ByteString,
    balance :: Integer
} deriving (Eq, Show, Read)

new_addr :: IO Address
new_addr = do
    keys <- regKeys
    let pubKey = hexPubKey_ $ fst keys
    return $ Address keys pubKey 50