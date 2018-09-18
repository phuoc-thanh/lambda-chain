{-# LANGUAGE OverloadedStrings #-}

module Cryptography where

import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Number.Serialize
import Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA as DSA
import qualified Crypto.PubKey.ECC.Generate as EG

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Base16
import Math.NumberTheory.Moduli.Sqrt


-- | Using Curve SEC-p256k1 for all DSA processes: y^2 = x^3 + 7
--
-- from bitcoin wiki: https://en.bitcoin.it/wiki/Secp256k1
-- secp256k1  refers to the parameters of the elliptic curve used in Bitcoin's public-key cryptography,
-- and is defined in Standards for Efficient Cryptography (SEC) (Certicom Research, http://www.secg.org/sec2-v2.pdf).
secp256k1 :: Curve
secp256k1 = getCurveByName SEC_p256k1

p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

type PublicKey = DSA.PublicKey
type PrivateKey = DSA.PrivateKey
type KeyPair = (DSA.PublicKey, DSA.PrivateKey)
type Signature = DSA.Signature

-- | Get an Eliptic Curve KeyPair, from the secp256k1 Curve
regKeys :: IO KeyPair
regKeys = EG.generate secp256k1
    -- return $ KeyPair secp256k1 (public_q p) (private_d s)

-- | Hex version, uncompressed form of Public Key from a Pair    
-- 64 hex bytes in long: 32 bytes of x-coordinate + 32 of y-coordinate
hexPubKey :: DSA.PublicKey -> ByteString
hexPubKey k = encode (i2osp x <> i2osp y) where
    Point x y = DSA.public_q k
    
-- | Hex version, compressed form of Public Key from a Pair    
-- 33 hex bytes in long: 1 flag-byte calculate from y + 32 bytes of x-coordinate
hexPubKey_ :: DSA.PublicKey -> ByteString
hexPubKey_ k = C8.append (trans y) $ encode (i2osp x) where
    Point x y = DSA.public_q k
    trans n = if mod n 2 == 0 then "02" else "03"

-- pubKey: 0362cfd7fe35d2f09ea56bd2af89c74833b1984aef25a34c80c62ed08a76170ec0
-- x = 44693887232528572152948022778881700161217984249896394545040971217292410425024
-- y = 3635561257270628387530222426536793713329471973488118665843926187645714183075

-- x = 60298221780214493849150366421388603287627659798797018133720152721044940792146
-- y = 23342128107284058945183914744268037289487212862355247319196082794995464121499

-- deHexKey_ :: ByteString -> Maybe DSA.PublicKey
-- deHexKey_ k = DSA.PublicKey secp256k1 (Point x y) where
--     y = y_from_x x
--     x = os2ip $ fst $ decode (C8.drop 2 k)

-- | Calculate y part from x
-- https://bitcoin.stackexchange.com/questions/48544/how-do-i-convert-public-key-x-value-to-y-in-python-and-verify
-- to-be-implement (case of even n odd)
y_from_x x = p - (sqrtModP' (mod y2 p) p) where y2 = x^3 + 7

hash :: ByteString -> Digest SHA256
hash m = hashWith SHA256 m

sign :: DSA.PrivateKey -> ByteString -> IO DSA.Signature
sign pk m = DSA.sign pk SHA256 m

verify :: DSA.PublicKey -> Signature -> ByteString -> Bool
verify = DSA.verify SHA256
