# lambda-chain

Haskell implementation of a simple blockchain

## Main goals

Lambda Chain main goals are below, ordered by priority.

1. Lambda Chain: Proof-of-Work [Completed]
The first part of research is to build a minimal viable blockchain, only core modules for demonstrating the research.

2. Lambda Chain: Cryptocurrency [Partial Completed]
The 2nd part of research, is to build a digital asset-like / electronic cash working on top of lambda chain. Nodes in chain are also have capale to handle memory pool and higher level of interaction.

3. Distributed Ledger Technology [On plan]
The completed chain should be a distributed database product, provide a small network with a append-only ledger(database)

4. Assets and Contracts

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

## Running Node

1. Load Dash interface
2. dash [port-number]
3. connect [host] [port]
4. transfer [address] [amount]
5. mine

