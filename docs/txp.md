This document is a draft of transaction processing documentation.
Probably it should be moved to another place (cardanodocs).

———————————————————————————————————————————————————————————————————

# Transaction processing

## Prerequisites

* You should know what a transaction is, its structure and about tx
  witnesses. You can read about it
  [here](https://cardanodocs.com/cardano/transactions/).
* You also should about technical details about
  [addresses](https://cardanodocs.com/cardano/addresses/).
* Please read about
  [balances and stakes](https://cardanodocs.com/cardano/balance-and-stake/) and
  don't confuse them.
* [Bootstrap era](https://cardanodocs.com/timeline/bootstrap/).
* [Block structure](https://cardanodocs.com/technical/blocks/#design).

## Overview

Transaction processing consists of two parts: global and local. Global
transaction processing is a part of block processing which checks
whether transactional payload of blocks is valid and updates
corresponding state when blocks are applied/rolled back. Local
transaction processing checks standalone transactions and updates
local mempool. These two parts have almost same logic with a few
differences. We first describe global transaction processing and then
describes how local transaction processing differs from it.

Transaction processing is abbreviated to `txp` in code and some other
places.

## Global transaction processing

Global transaction processing can be described by presenting an
algorithm to solve the following problem: given a sequence of blocks
`B₀, B₁, B₂, …` (where `B₀` is the first genesis block) check whether
transactions from these blocks are valid. The algorithm is similar to
the one from Bitcoin (for example, UTXO is used to prevent
double-spending), but is more complicated because there are more
features.

For verification we maintain some state (called GState) which
corresponds to the application of a sequence of blocks. It's associated
with the hash of the header of the last applied block. So, given a
sequence of blocks `B₀, B₁, B₂, …` we can compute sequence of states
`S₀, S₁, S₂, …`. There is also genesis state `G` which takes places
before any block is applied. This state is derived from genesis data
(see ???).

In theory transaction verification can be stateless (except genesis
state `G`) and be described without mentioning any additional
state. But in practice it would be very inefficient and inconvenient,
so we present the verification algorithm in a stateful way.

The problem: given GState `S` and a block `B` return either an error
describing why `B` is invalid (w.r.t. tx payload) or new GState
`S'`. Genesis blocks don't have tx payload and are ignored by global
txp. Recall that tx payload contains transactions (stored in Merkle
tree) and their witnesses.

### GState

In this section we describe parts of GState relevant to transaction
processing.

* UTXO (unspent transaction outputs). It's a map from `TxIn` to
  `TxOutAux` which contains all unspent outputs. `TxOutAux` is just an
  alias for `TxOut`. Later it can be extended if we want to associate
  more data with unspent outputs (e. g. slot of the block in which
  this transaction appeared).
  For example, if a transaction `A` has 1 output (`out1`) which hasn't been
  spent yet, UTXO will have a pair `(TxIn (hash A) 0, out1)`.
* Stakes. They are not needed for tx verification, but they are needed
  for other parts of the protocol (for example, leader election
  process called FTS) and they are maintained by tx processing.
  We store total stake and a map from `StakeholderId` to its stake (`Coin`).
* Adopted `BlockVersionData`. This data type contains various
  parameters of the algorithm which can be updated by Update
  System. Some values are relevant for transaction processing:
  * `maxTxSize` – maximal size of a transaction.
  * `scriptVersion` – determines the rules which should be used to
    verify scripts.
  * `txFeePolicy` – determines the rules to check whether a
    transaction has enough fees.
  * `unlockStakeEpoch` – determines epoch when bootstrap era ends.

### Inputs checks

* Each input must exist in UTXO. Further we assume that for each input
  we also know corresponding output (if we don't, this check fails).
* All inputs are different.
* Each input is properly certified by its witness. **TODO**: elaborate.

**TODO**: describe other checks.

### GState modifications

If all checks above pass, we modify GState appropriately.

**TODO**: describe modifications of UTXO and stakes.

## Local transaction processing

Local transaction processing is needed for transaction relay. Node can
receive a standalone transaction from the network and then it needs to
verify it again its current state (global + mempool) and apply or
reject. If it's applied, it's also relayed further, so that other
nodes in network become aware of a valid transaction. There are few
differences between local txp and global txp:

1. In local txp we consider not only GState, but also mempool. We
   behave as if transactions in mempool were applied as part of a
   block. We never modify GState, only in-memory data is modified.
2. We also have a limit on mempool size, specified in a number of
   transactions. If mempool is overwhelmed, we won't accept new
   transactions until we free up some space.
3. Transaction verification depends on epoch. In global txp we know
   epoch from block header. In local txp we use current epoch. If it's
   unknown (which means that our local chain is quite outdated), we
   reject incoming transactions.

Note that local transaction processing is basically an implementation
details and other nodes can do it differently. For example, a node can
always reject all transactions and never relay them (which is bad).
