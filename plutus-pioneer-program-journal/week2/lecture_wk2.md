# Recap

There are two sides to a smart contract:
* On-chain part
    * Allows nodes to validate transactions involving the smart contract
    * Transactions are only allowed to consume valid UTXOs
* Off-chain part
    * Lives in users wallet and constracts/submits suitable transactions


# On-chain process
EUTXO model introduces a new type of addresses. The type of addresses used in the UTXO model is simply public-key addresses. If a UTXO sits at that public-key address than a transaction is able to consume that UTXO as an input if the signature belonging to that public-key is included in the transaction

The EUTXO model extends this by adding a new type of address, called a "script" address which are able to run arbitrary logic. Transactions looking to consume UTXOs sitting at a script address are validated by a node, the node will run the script and then depending on the result of the script, decide whether the transaction is valid or not

On the script side sits some "datum" which is essentially a small slice of the state of the blockchain

There are three pieces of data that a plutus script gets:
1. the datum
2. the redeemer
3. the context

All three pieces of data use the same Haskell datatype. 

# Instructions to see datatype Data
1. Ensure you `git checkout` the correct `tag` for week 2
2. Start a `nix-shell` in `cardano/plutus`
3. `cd` to `cardano/plutus-pioneer-program/code/week02`
4. run `cabal repl`
    * run `cabal update` if repl fails
5. run `import PlutusTx`
6. Enter `i: Data` to get information on the datatype `Data`
7. Enter `:set -XOverloadedStrings` which allows us to use byte strings (and the B constructor)

# Instructions to write first validator
1. Create a new Haskell module
2. 