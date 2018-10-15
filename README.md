# lambda-chain

Haskell implementation of a simple blockchain

## Install Lightning DB (lmdb)

Via Package Manager on Unix system (brew, yum, apt...)
> brew install lmdb

 or build from [vcpkg](https://github.com/Microsoft/vcpkg) on Windows

## Packages needed

1. encode/decode and crypto
> $ cabal install base16-bytestring cryptonite merkle-tree arithmoi

2. hashmap and system path
> $ cabal install unordered-containers easy-file

3. database and network
> $ cabal install lmdb network