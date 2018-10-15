{-# LANGUAGE OverloadedStrings #-}

module Crypto (
    DSA.PublicKey,
    DSA.PrivateKey,
    DSA.Signature,
    Crypto.verify,
    Crypto.hash,
    Crypto.sign,
    Digest,
    SHA256,
    KeyPair(..),
    MerkleTree,
    MerkleRoot,
    merkle_root,
    regKeys,
    hexPubKey_,
    getPubKey_
) where

import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Number.Serialize
import Crypto.Hash
import Crypto.Hash.MerkleTree
import qualified Crypto.PubKey.ECC.ECDSA as DSA
import qualified Crypto.PubKey.ECC.Generate as EG

import Data.Monoid
import Data.ByteString (ByteString)
import Data.ByteString.Char8
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

type KeyPair = (DSA.PublicKey, DSA.PrivateKey)

-- | Register an Eliptic Curve KeyPair, from the secp256k1 Curve
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
hexPubKey_ k = append (trans y) $ encode (i2osp x) where
    Point x y = DSA.public_q k
    trans n = if even n then "02" else "03"

-- | Extract public hex-address to Original Public Key
getPubKey_ :: ByteString -> Maybe DSA.PublicKey
getPubKey_ k = if (isPointValid_ x y) then Just $ DSA.PublicKey secp256k1 (Point x y) else Nothing where
    y = y_from_x f x
    x = os2ip . fst $ decode s
    (f, s) = Data.ByteString.Char8.splitAt 2 k

-- | Calculate y-coordinate from a flag & x-coordinate
y_from_x "02" x
    | even (sqrt_mod x) = sqrt_mod x
    | otherwise = p - sqrt_mod x
y_from_x "03" x
    | odd (sqrt_mod x) = sqrt_mod x
    | otherwise = p - sqrt_mod x
y_from_x _ x = 0

-- | Modular Square Root, hard to explain..
sqrt_mod x = sqrtModP' (mod y2 p) p where y2 = x^3 + 7

-- | Check if (Point x y) is valid with secp256k1 Curve or not 
isPointValid_ x y = isPointValid secp256k1 (Point x y)

-- | Hash raw data with SHA256 hash function
hash :: ByteString -> Digest SHA256
hash m = hashWith SHA256 m

-- | Return mekle root of hashed leaves
merkle_root :: [ByteString] -> ByteString
merkle_root = mtHash . mkMerkleTree

-- | Sign with SHA256 hash function
sign :: DSA.PrivateKey -> Digest SHA256 -> IO DSA.Signature
sign pk m = DSA.sign pk SHA256 m

-- | Verify with SHA256 hash function
verify :: DSA.PublicKey -> DSA.Signature -> Digest SHA256 -> Bool
verify = DSA.verify SHA256
