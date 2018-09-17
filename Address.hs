{-# LANGUAGE OverloadedStrings #-}

module Address where

import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Types
import Crypto.Number.Serialize
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as EG
import qualified Crypto.PubKey.ECC.Types as ECC

import Data.Monoid
import Data.ByteString (ByteString)
import Data.ByteString.Char8
import Data.ByteString.Base16 

-- | Using Curve SEC-p256k1 for all ECDSA processes
--
-- from bitcoin wiki: https://en.bitcoin.it/wiki/Secp256k1
-- secp256k1  refers to the parameters of the elliptic curve used in Bitcoin's public-key cryptography,
-- and is defined in Standards for Efficient Cryptography (SEC) (Certicom Research, http://www.secg.org/sec2-v2.pdf).
secp256k1 :: Curve
secp256k1 = getCurveByName SEC_p256k1

-- | Get an Eliptic Curve KeyPair, from the secp256k1 Curve
regKeys :: IO KeyPair
regKeys = do
    (p, s) <- EG.generate secp256k1
    return $ KeyPair secp256k1 (public_q p) (private_d s)

-- | Hex version, uncompressed form of Public Key from a Pair    
-- 64 hex bytes in long: 32 bytes of x-coordinate + 32 of y-coordinate
hexPubKey :: KeyPair -> ByteString
hexPubKey k = encode (i2osp x <> i2osp y) where
    Point x y = public_q $ toPublicKey k
    
-- | Hex version, compressed form of Public Key from a Pair    
-- 33 hex bytes in long: 1 flag-byte calculate from y + 32 bytes of x-coordinate
hexPubKey_ :: KeyPair -> ByteString
hexPubKey_ k = append (trans y) $ encode (i2osp x) where
    Point x y = public_q $ toPublicKey k
    trans n = if mod n 2 == 0 then "02" else "03"

data Address = Address {
    keyPair :: KeyPair,
    pubKey  :: ByteString,
    balance :: Integer
} deriving (Eq, Show, Read)

regAddress :: IO Address
regAddress = do
    keys <- regKeys
    let pubKey = hexPubKey_ keys
    return $ Address keys pubKey 50